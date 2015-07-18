#!/usr/bin/env python
from __future__ import print_function, unicode_literals
import io, json, datetime
open = io.open

def dict_update(d1, d2):
    '''Merge two dicts and returns the result.  The original dicts are
    unchanged.'''
    d = dict(d1)
    d.update(d2)
    return d

def json_load_file(filename, json_args={}, **open_args):
    import json
    with open(filename, **open_args) as f:
        return json.load(f, **json_args)

def json_dump_file(filename, data, json_args={}, **open_args):
    import json
    with open(filename, "w", **open_args) as f:
        json.dump(data, f, **dict_update(JSON_FORMAT, json_args))

def search_json(pred, data):
    '''Search a JSON object for data that matches the given predicate:
    pred : (Str | Int | Float | Bool | None) -> BoolLike

    Example:
        tuple(search_json(lambda x: isinstance(x, str) and
                          "So how are the pooflemeefs tonight?" in x, data))
    '''
    if isinstance(data, dict):
        for k, v in data.items():
            if pred(k):
                yield [k]
            for m in search_json(pred, v):
                yield [k] + m
    elif isinstance(data, list):
        for i, x in enumerate(data):
            for m in search_json(pred, x):
                yield [i] + m
    else:
        if pred(data):
            yield []

def find_conversation(pred, data):
    '''Search for a given conversation whose metadata matches the given
    predicate:
    pred :: {"id": Str, "name"?: Str, ...} -> BoolLike
    '''
    return (c for c in data["conversation_state"]
            if pred(c["conversation_state"]["conversation"]))

def extract_participants(conversation):
    metadata = conversation["conversation_state"]["conversation"]
    participants = metadata["participant_data"]
    return dict(
        (participant["id"]["gaia_id"], participant.get("fallback_name", None))
        for participant in participants
    )

def expect(b, s, x=None):
    import sys
    if not b:
        sys.stderr.write("Unexpected: " + repr(s) + "\n")
        assert False
    return x

def extract_messages(conversation, participants):
    events = conversation["conversation_state"]["event"]
    messages = []
    for event in events:
        event_type = event["event_type"]
        sender_id = event["sender_id"]["gaia_id"]
        message = {
            "timestamp": event["timestamp"],
            "sender_id": sender_id,
            "sender_name": participants.get(sender_id, None),
        }
        if event_type == "REGULAR_CHAT_MESSAGE":
            content = event["chat_message"]["message_content"]
            text = "".join(
                expect(s["type"] == "LINE_BREAK" or "text" in s,
                       s, s.get("text", "\n"))
                for s in content["segment"]
            ) if "segment" in content else "\n".join(
                expect(tuple(a["embed_item"]["type"]) == ("PLUS_PHOTO",),
                       a, a["embed_item"]["embeds.PlusPhoto.plus_photo"]["url"])
                for a in content["attachment"]
            )
            message.update({
                "type": "message",
                "text": text,
            })
        elif event_type == "ADD_USER":
            action = event["membership_change"]
            if action["type"] == "JOIN":
                message.update({
                    "type": "join",
                    "participants": tuple(p["gaia_id"]
                                          for p in action["participant_id"]),
                })
            else:
                expect(False, event)
        elif event_type == "REMOVE_USER":
            action = event["membership_change"]
            if action["type"] == "LEAVE":
                message.update({
                    "type": "leave",
                    "participants": tuple(p["gaia_id"]
                                          for p in action["participant_id"]),
                })
            else:
                expect(False, event)
        elif event_type == "RENAME_CONVERSATION":
            action = event["conversation_rename"]
            message.update({
                "type": "rename",
                "old_name": action["old_name"],
                "new_name": action["new_name"],
            })
        elif event_type == "HANGOUT_EVENT":
            action = event["hangout_event"]
            if action["event_type"] == "START_HANGOUT":
                message.update({
                    "type": "start_hangout",
                })
            elif action["event_type"] == "END_HANGOUT":
                message.update({
                    "type": "end_hangout",
                    "participants": tuple(p["gaia_id"]
                                          for p in action["participant_id"]),
                })
            else:
                expect(False, event)
        else:
            expect(False, event)
        messages.append(message)

    messages.sort(key=lambda x: x["timestamp"])
    return messages

def extract_participants_and_messages(data, conversation_filter):
    '''Extract the participants and messages from the full Hangouts JSON dump.
    data                 input JSON for Hangouts data
                         (obtained via Google Takeout)
    conversation_filter  a predicate to filter the conversations
                         (see 'find_conversation')
    '''

    conversations = tuple(find_conversation(conversation_filter, data))
    if len(conversations) == 0:
        raise Exception("No conversations match the given filter.")
    elif len(conversations) > 1:
        raise Exception("More than one conversation match the given filter.")
    conversation = conversations[0]

    participants = extract_participants(conversation)
    return (
        participants,
        extract_messages(conversation, participants),
    )

def analyze_reply_distribution(participants, messages, max_time):
    import math
    import numpy as np

    participant_names = sorted(set(participants.values()))
    participant_names_r = dict((n, i) for i, n in enumerate(participant_names))
    participant_ids_r = dict((g, participant_names_r[n])
                             for g, n in participants.items())
    num_participants = len(participant_names)

    print("analyzing...", end="")
    distribution = [[] for _ in range(num_participants * num_participants)]
    for i, message1 in enumerate(messages):
        time1 = float(message1["timestamp"]) / 1e6
        sender1 = participant_ids_r.get(message1["sender_id"], None)
        if sender1 is None:
            continue
        for j in range(i + 1, len(messages)):
            message2 = messages[j]
            time2 = float(message2["timestamp"]) / 1e6
            dtime = time2 - time1
            if dtime >= max_time:
                break
            sender2 = participant_ids_r.get(message2["sender_id"], None)
            if sender2 is None:
                continue
            distribution[sender1 * num_participants + sender2].append(dtime)
        print("\ranalyzing... {0:6.1f}%"
              .format(i * 100. / len(messages)), end="")
    print()

    for d in distribution:
        d.sort()
    return participant_names, distribution

def normalize_array(data, weights):
    import numpy as np
    return data / np.sum(data * weights)

def simple_gamma_kernel(theta):
    '''Convolution kernel using the gamma distribution, with the scale
    parameter (theta) fixed.'''
    import numpy as np
    import scipy.integrate
    import scipy.stats
    @np.vectorize
    def kernel(target_x, x_begin, x_end):
        # x = .5 * (x_begin + x_end)
        # return scipy.stats.gamma.pdf(target_x, a=(x / theta), scale=theta)
        scale = np.sqrt(theta ** 2 + (x_begin - x_end) ** 2 / 4.)
        def f(x):
            return scipy.stats.gamma.pdf(target_x, a=(x / scale), scale=scale)
        return scipy.integrate.quad(f, x_begin, x_end)[0] / (x_end - x_begin)
    return kernel

def smoothing_matrix(target_xs, xs_begin, xs_end, kernel):
    return kernel(target_xs[:, None], xs_begin, xs_end) * (xs_end - xs_begin)

def get_sender_name(sender_id, participants, unknown_senders=None):
    try:
        return participants[sender_id]
    except KeyError:
        pass
    if unknown_senders is not None:
        unknown_senders.add(sender_id)
    return "*user_{0}".format(sender_id)

def export_messages_text(f, messages, participants):
    '''Export the textual content of the messages as a human readable file.
    Return a set of unknown senders (without names).'''
    unknown_senders = set()
    f.write("# all times are UTC\n")
    for m in messages:
        time = round(float(m["timestamp"]) / 1e6)
        time = datetime.datetime.utcfromtimestamp(time).isoformat(str(" "))
        f.write("[{0}] ".format(time))
        sender = get_sender_name(m["sender_id"], participants, unknown_senders)
        if m["type"] == "message":
            lines = m["text"].split("\n")
            f.write("<{0}> {1}\n".format(sender, lines[0]))
            for line in lines[1:]:
                f.write(" " * 20 + "| {0}\n".format(line))
        elif m["type"] == "join":
            f.write("{0} invites {1}\n".format(
                sender,
                ", ".join(
                    get_sender_name(p, participants, unknown_senders)
                    for p in m["participants"]
                ),
            ))
        elif m["type"] == "leave":
            f.write("{0} leaves\n".format(sender))
        elif m["type"] == "rename":
            f.write("{0} renames the chat from {1} to {2}\n".format(
                sender,
                m["old_name"],
                m["new_name"],
            ))
        elif m["type"] == "start_hangout":
            f.write("{0} starts a video call\n".format(sender))
        elif m["type"] == "end_hangout":
            f.write("{0} ends a video call with {1}\n".format(
                sender,
                ", ".join(
                    get_sender_name(p, participants, unknown_senders)
                    for p in m["participants"]
                ),
            ))
        else:
            raise Exception("unknown message type: " + repr(m["type"]))
    return unknown_senders


def main():
    import matplotlib.pyplot as plt
    import numpy as np
    import scipy.stats

    data_filename          = "hangouts.json"
    participants_filename  = "hangouts-participants.json"
    messages_filename      = "hangouts-messages.json"
    messages_text_filename = "hangouts-messages.txt"
    names_filename         = "participant-names.json"
    distribution_filename  = "reply-distribution.json"
    # format of compare-names.json: {"focus": Name, "others": [Name]}
    compare_names_filename = "compare-names.json"
    base = 1.5
    min_exponent = -4                   # inclusive
    max_exponent = 28                   # exclusive

    def reduce_data(generate_participants_file=True):
        '''Requires: hangouts.json and,
        if generate_participants_file is False, hangouts-participants.json'''
        data = json_load_file(data_filename)
        participants, messages = extract_participants_and_messages(
            data,
            conversation_filter=lambda c: "Pripyat" in c.get("name", ""),
        )
        if generate_participants_file:
            json_dump_file(participants_filename, participants)
        json_dump_file(messages_filename, messages)
    #reduce_data(generate_participants_file=False)

    def export_messages():
        '''Requires: output from reduce_data and hangouts-participants.json.'''
        print("exporting messages to {0}...".format(messages_text_filename))
        messages     = json_load_file(messages_filename)
        participants = json_load_file(participants_filename)
        with open(messages_text_filename, "wt", encoding="utf8") as f:
            unknown_senders = export_messages_text(f, messages, participants)
        print("note: the following participants do not have a name:")
        print("\n".join(unknown_senders))
    #export_messages()

    def analyze_data():
        '''Requires: output from reduce_data.'''
        participants = json_load_file(participants_filename)
        messages     = json_load_file(messages_filename)
        participant_names, distribution = analyze_reply_distribution(
            participants,
            messages,
            max_time=86400.,
        )
        json_dump_file(distribution_filename, distribution)
        json_dump_file(names_filename, participant_names)
    #analyze_data()

    def visualize_data():
        '''Requires: output from analyze_data and compare-names.json.'''
        participant_names = json_load_file(names_filename)
        distribution      = json_load_file(distribution_filename)
        compare_names     = json_load_file(compare_names_filename)
        focus  = compare_names["focus"]
        others = compare_names["others"]
        # t       = np.logspace(min_exponent, max_exponent, 100, base=base)
        # t_begin = base ** np.arange(min_exponent, max_exponent)
        # t_mid   = base ** (np.arange(min_exponent, max_exponent) + .5)
        # t_end   = base ** (np.arange(min_exponent, max_exponent) + 1)
        # weights = t_begin
        for name in others:
            t = distribution[
                participant_names.index(focus) * len(participant_names) +
                participant_names.index(name)
            ]
            d = np.arange(len(t)) / len(t)
            plt.plot(t, d, label=name)
        plt.xscale("log")
        plt.yscale("log")
        plt.xlabel("time between messages /sec")
        plt.ylabel("cumulative probability")
        plt.legend()
        plt.show()
        # todo: figure out how to subtract the uncorrelated background
        # todo: do a better job smoothing
        # todo: reduce the bias from older data
    #visualize_data()

JSON_FORMAT = {"sort_keys": True, "indent": 4, "separators": (',', ': ')}

if __name__ == "__main__":
    main()
