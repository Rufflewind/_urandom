#!/usr/bin/env python
#
# Python script that automatically deduces and adds type signatures to a file
# containing Haskell source code. Warning: very hacky implementation that
# doesn't always work, requires GHCi and still requires manual postprocessing!

import os, re, subprocess, sys

ENCODING = "utf-8"

keywords = '''
{-#
#-}
{-
-}
--
class
data
foreign
import
infix
infixl
infixr
instance
module
newtype
type
'''[1:-1].encode(ENCODING).split()

CUSTOM_PROMPT = b"zC1sRi6p8qsfom7vzVaJlK0jWvIR3HATRRhIQUZS"

class Interpreter(object):

    def __init__(self, args=()):
        self._args = ["ghci"]
        self._args.extend(args)
        self._process = None

    def __enter__(self):
        self._process = subprocess.Popen(
            self._args,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
        )
        self._write(b":set prompt " + CUSTOM_PROMPT)
        self._read()
        return self

    def __exit__(self, type, value, traceback):
        self._write(b":q")
        self._process.stdin.close()
        self._process.wait()

    def _read(self, prompt=CUSTOM_PROMPT):
        start_index = -len(prompt)
        chars = bytearray()
        while True:
            char = self._process.stdout.read(1)
            chars.extend(char)
            if not char:
                break
            try:
                if chars[start_index:] == CUSTOM_PROMPT:
                    del chars[start_index:]
                    break
            except IndexError:
                pass
        return bytes(chars)

    def _write(self, string):
        self._process.stdin.write(string)
        self._process.stdin.write(b"\n")
        self._process.stdin.flush()

    def modify_modules(self, *module_names, action=""):
        '''action is one of "", "+", "-"'''
        self._write(b":module " + action + " " + b" ".join(module_names))
        self._read()

    def module_names(self):
        self._write(b":show modules")
        lines = self._read().split(b"\n")
        def f(line):
            m = re.match(b"^\w+", line)
            if m:
                return m.group(0)
        return tuple(filter(lambda x: x, map(f, lines)))


    def symbol_names(self, *module_names):
        self._write(b":browse " + b" ".join(module_names))
        lines = self._read().split(b"\n")
        def f(line):
            m = re.match(b"^\S+", line)
            if not m:
                return
            name = m.group(0)
            if name in keywords:
                return
            return name
        return tuple(filter(lambda x: x, map(f, lines)))

    def type_of(self, symbol_name):
        self._write(b":type " + symbol_name)
        return self._read()

    def symbol_source(self, symbol_name):
        self._write(b":list " + symbol_name)
        lines = self._read().split(b"\n")
        return lines

def insert_before_line(numbered_lines, line_number, string):
    for index, (current_line_number, _) in enumerate(numbered_lines):
        if current_line_number == line_number:
            numbered_lines.insert(index, (-1, string))
            break
    else:
        raise Exception("can't find line number {0}".format(line_number))

in_filename = "Lexer.hs"
out_filename = "Lexer.out.hs"
interpreter_args = (in_filename,)

with open(in_filename) as in_file:
   numbered_lines = list(enumerate(in_file))

with Interpreter(interpreter_args) as interpreter:
    module_name = interpreter.module_names()[0]
    print("processing module {0}...".format(module_name.decode(ENCODING)))
    for symbol_name in interpreter.symbol_names(module_name):
        # line number of the definition
        m = re.match(b"\d+", interpreter.symbol_source(symbol_name)[1])
        line_number = int(m.group(0)) - 1        # our numbers start from zero
        insert_before_line(
            numbered_lines,
            line_number,
            interpreter.type_of(symbol_name).decode(ENCODING)
        )

with open(out_filename, "w") as out_file:
    for _, line in numbered_lines:
        out_file.write(line)
