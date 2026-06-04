#!/usr/bin/env python3

# Usage:
#   echo 'ls -alh | grep foo > file.txt' | localmost ast | ./etc/ast2json.py
#
# Script written by Claude Opus 4.8. This file is not part of localmost itself.

import json
import sys


class Parser:
    def __init__(self, text):
        self.s = text
        self.i = 0
        self.n = len(text)

    def peek(self):
        return self.s[self.i] if self.i < self.n else ""

    def skip_ws(self):
        while self.i < self.n and self.s[self.i] == " ":
            self.i += 1

    def expect(self, ch):
        if self.peek() != ch:
            raise ValueError(
                f"expected {ch!r} at offset {self.i}: "
                f"...{self.s[self.i : self.i + 30]!r}"
            )
        self.i += 1

    def at_end(self):
        self.skip_ws()
        return self.i >= self.n

    def parse_value(self):
        self.skip_ws()
        c = self.peek()
        if c == '"':
            return self.parse_string()
        if c == "[":
            return self.parse_list()
        if c == "(":
            return self.parse_paren()
        if c[:1].isalpha() and c.isupper():
            return self.parse_constructor()
        return self.parse_atom()

    def parse_string(self):
        self.expect('"')
        out = []
        while self.i < self.n:
            c = self.s[self.i]
            if c == "\\" and self.i + 1 < self.n:
                out.append(self.s[self.i + 1])
                self.i += 2
                continue
            if c == '"':
                self.i += 1
                return "".join(out)
            out.append(c)
            self.i += 1
        raise ValueError("unterminated string")

    def parse_list(self):
        self.expect("[")
        items = []
        self.skip_ws()
        if self.peek() == "]":
            self.i += 1
            return items
        while True:
            items.append(self.parse_value())
            self.skip_ws()
            if self.peek() == ",":
                self.i += 1
                continue
            self.expect("]")
            return items

    def parse_paren(self):
        # Either a grouped value `(x)` or a tuple `(x, y, ...)`.
        self.expect("(")
        items = [self.parse_value()]
        self.skip_ws()
        while self.peek() == ",":
            self.i += 1
            items.append(self.parse_value())
            self.skip_ws()
        self.expect(")")
        return items[0] if len(items) == 1 else items

    def parse_constructor(self):
        name = self.parse_conname()

        # Record syntax: `Con {field = value, ...}`.
        self.skip_ws()
        if self.peek() == "{":
            return self.parse_record(name)

        # Constructor application: collect following args until a terminator.
        args = []
        while True:
            self.skip_ws()
            c = self.peek()
            if c == "" or c in ",)]}":
                break
            args.append(self.parse_value())

        return self.build_conapp(name, args)

    def parse_record(self, name):
        self.expect("{")
        obj = {"type": name}
        while True:
            self.skip_ws()
            field = self.parse_fieldname()
            self.skip_ws()
            self.expect("=")
            obj[field] = self.parse_value()
            self.skip_ws()
            if self.peek() == ",":
                self.i += 1
                continue
            self.expect("}")
            return obj

    def build_conapp(self, name, args):
        # `Id N` -> the bare integer (ids are noise as objects).
        if name == "Id" and len(args) == 1:
            return args[0]

        # `OuterToken (Id N) inner` -> inner node tagged with its id.
        if name == "OuterToken" and len(args) == 2:
            ident, inner = args
            if isinstance(inner, dict):
                inner = {"id": ident, **inner}
            elif isinstance(inner, str):
                # A nullary inner node, e.g. `Inner_T_Greater`.
                inner = {"id": ident, "type": inner}
            else:
                inner = {"id": ident, "node": inner}
            return inner

        if not args:
            return self.normalize_nullary(name)

        return {"type": self.strip_prefix(name), "args": args}

    def parse_conname(self):
        start = self.i
        while self.i < self.n and (self.s[self.i].isalnum() or self.s[self.i] in "_'"):
            self.i += 1
        return self.s[start : self.i]

    def parse_fieldname(self):
        return self.parse_conname()

    def parse_atom(self):
        start = self.i
        depth = 0
        while self.i < self.n:
            c = self.s[self.i]
            if c == '"' or c == "[":
                break
            if c in "({":
                depth += 1
            elif c in ")}]":
                if depth == 0:
                    break
                depth -= 1
            elif c == "," and depth == 0:
                break
            self.i += 1
        token = self.s[start : self.i].rstrip()
        return self.coerce_atom(token)

    @staticmethod
    def coerce_atom(token):
        if token == "True":
            return True
        if token == "False":
            return False
        try:
            return int(token)
        except ValueError:
            return token

    @staticmethod
    def strip_prefix(name):
        for p in ("Inner_",):
            if name.startswith(p):
                name = name[len(p) :]
        return name

    def normalize_nullary(self, name):
        name = self.strip_prefix(name)
        if name == "Nothing":
            return None
        if name == "True":
            return True
        if name == "False":
            return False
        return name


def parse_show(text):
    p = Parser(text)
    value = p.parse_value()
    if not p.at_end():
        raise ValueError(f"trailing input at offset {p.i}: ...{p.s[p.i : p.i + 30]!r}")
    return value


def main():
    lines = [ln.rstrip("\n") for ln in sys.stdin if ln.strip()]
    if not lines:
        sys.exit("ast2json.py: no input on stdin")

    result = {}
    try:
        result["shellcheckAst"] = parse_show(lines[0])
        if len(lines) > 1:
            result["script"] = parse_show(lines[1])
    except ValueError as e:
        sys.exit(f"ast2json.py: parse error: {e}")

    json.dump(result, sys.stdout, indent=2)
    sys.stdout.write("\n")


if __name__ == "__main__":
    main()
