#!/usr/bin/env python
from __future__ import unicode_literals
import json, math, os, random, sys, time

try:
    input = raw_input
except NameError:
    pass

def get_input(parse, default=None):
    while True:
        if default is not None:
            sys.stdout.write("[default: {0}] ".format(default))
        sys.stdout.write("> ")
        sys.stdout.flush()
        s = input().strip()
        if default is not None and not s:
            s = default
        try:
            x = parse(s)
        except ValueError as e:
            if s:
                sys.stderr.write("*** {0}\n".format(e))
                sys.stderr.flush()
        else:
            return x

def get_score(num_correct, num_questions, total_time):
    num_correct   = float(num_correct)
    num_questions = float(num_questions)
    return (1e3 * num_questions / total_time *
            (1 - math.exp(-(num_questions / 10.) ** 2)) *
            (num_correct / num_questions) ** 4)

JSON_FORMAT = {"ensure_ascii": False, "sort_keys": True,
               "indent": 4, "separators": (',', ': ')}

def int_or_undo(s):
    if s.lower() == "undo":
        return "undo"
    try:
        return int(s)
    except ValueError:
        raise ValueError("not a number")

if len(sys.argv) == 2:
    filename = sys.argv[1]
elif len(sys.argv) == 1:
    filename = os.path.splitext(__file__)[0] + "-results.json"
else:
    sys.stderr.write("usage: {0} [OUTPUT_FILENAME]\n".format(__file__))
    exit(2)

questions = [("{0} \xd7 {1} = ?".format(x, y), x * y)
             for x in range(13) for y in range(13)]

random.seed()
random.shuffle(questions)

sys.stdout.write("How many questions?\n")
num_questions = get_input(int, default="100")

sys.stdout.write("Press [Enter] to begin.  (Type 'undo' to undo.)\n")
get_input(lambda x: x)
sys.stdout.write("-" * 40 + "\n")

history = []
i = 0
try:
    while i < min(num_questions, len(questions)):
        question, expected = questions[i]
        sys.stdout.write("\n" + question + "\n")
        time_init = time.time()
        answer = get_input(int_or_undo)
        time_taken = time.time() - time_init
        if answer == "undo":
            correct = None
        else:
            correct = answer == expected
        if i < len(history):
            history[i].update({
                "answer": answer,
                "time": history[i]["time"] + time_taken,
                "correct": correct,
            })
        else:
            history.append({
                "question": question,
                "expected": expected,
                "answer": answer,
                "time": time_taken,
                "correct": correct,
            })
        if answer == "undo":
            i -= 1
        else:
            i += 1
except KeyboardInterrupt:
    pass

num_questions = len(history)
if num_questions == 0:
    exit()
num_correct = sum(int(x["correct"]) for x in history)
total_time = sum(x["time"] for x in history)

sys.stdout.write("\n")
sys.stdout.write("-" * 40 + "\n")
sys.stdout.write("Time: {0:.1f} s ({1:.0f} ms/question)\n"
                 .format(total_time, total_time * 1e3 / num_questions))
sys.stdout.write("Grade: {0}/{1} correct ({2:.1f}%).\n"
                 .format(num_correct, num_questions,
                         num_correct * 100. / num_questions))

if num_correct != num_questions:
    sys.stdout.write("Wrong answers:\n")
for x in history:
    if x["correct"]:
        continue
    sys.stdout.write("  {0}  -->  {1} (your answer: {2}).\n"
                     .format(x["question"], x["expected"], x["answer"]))

sys.stdout.write(
    "Overall score: {0} (higher the better)\n"
    .format(round(get_score(num_correct, num_questions, total_time))))

with open(filename, "wb") as f:
    f.write(json.dumps(history, **JSON_FORMAT).encode("utf8"))
sys.stdout.write("Results written to {0}\n".format(filename))
