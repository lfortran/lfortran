"""Minimal EDN tree utilities for ASR mutation reduction."""

import copy
import dataclasses
import re


@dataclasses.dataclass
class Node:
    kind: str
    value: str = ""
    children: list = dataclasses.field(default_factory=list)

    def clone(self):
        return copy.deepcopy(self)


class ParseError(ValueError):
    pass


class Parser:
    def __init__(self, text):
        self.text = text
        self.position = 0

    def skip_space(self):
        while self.position < len(self.text):
            char = self.text[self.position]
            if char.isspace() or char == ",":
                self.position += 1
            elif char == ";":
                newline = self.text.find("\n", self.position)
                self.position = len(self.text) if newline < 0 else newline + 1
            else:
                break

    def parse(self):
        self.skip_space()
        result = self.parse_value()
        self.skip_space()
        if self.position != len(self.text):
            raise ParseError(
                f"trailing input at offset {self.position}")
        return result

    def parse_value(self):
        self.skip_space()
        if self.position >= len(self.text):
            raise ParseError("expected a value")
        char = self.text[self.position]
        if char == "(":
            return self.parse_container("list", "(", ")")
        if char == "[":
            return self.parse_container("vector", "[", "]")
        if char == "{":
            return self.parse_container("map", "{", "}")
        if char == '"':
            return self.parse_string()
        if char == "#":
            return self.parse_tag()
        return self.parse_atom()

    def parse_container(self, kind, opening, closing):
        if self.text[self.position] != opening:
            raise ParseError(f"expected {opening}")
        self.position += 1
        children = []
        while True:
            self.skip_space()
            if self.position >= len(self.text):
                raise ParseError(f"unterminated {opening}")
            if self.text[self.position] == closing:
                self.position += 1
                if kind == "map" and len(children) % 2 != 0:
                    raise ParseError("map requires key/value pairs")
                return Node(kind, children=children)
            children.append(self.parse_value())

    def parse_string(self):
        start = self.position
        self.position += 1
        escaped = False
        while self.position < len(self.text):
            char = self.text[self.position]
            self.position += 1
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == '"':
                return Node("atom", self.text[start:self.position])
        raise ParseError("unterminated string")

    def parse_tag(self):
        self.position += 1
        start = self.position
        while (self.position < len(self.text) and
               not self.text[self.position].isspace() and
               self.text[self.position] not in "()[]{}\",;"):
            self.position += 1
        if start == self.position:
            raise ParseError("empty tag")
        tag = self.text[start:self.position]
        return Node("tag", tag, [self.parse_value()])

    def parse_atom(self):
        start = self.position
        while (self.position < len(self.text) and
               not self.text[self.position].isspace() and
               self.text[self.position] not in "()[]{}\",;"):
            self.position += 1
        if start == self.position:
            raise ParseError(
                f"unexpected character {self.text[self.position]!r}")
        return Node("atom", self.text[start:self.position])


def parse(text):
    return Parser(text).parse()


def render(node):
    if node.kind == "atom":
        return node.value
    if node.kind == "tag":
        return f"#{node.value} {render(node.children[0])}"
    delimiters = {
        "list": ("(", ")"),
        "vector": ("[", "]"),
        "map": ("{", "}"),
    }
    opening, closing = delimiters[node.kind]
    return opening + " ".join(render(child) for child in node.children) + closing


def walk(node, path=()):
    yield path, node
    for index, child in enumerate(node.children):
        yield from walk(child, path + (index,))


def at_path(node, path):
    current = node
    for index in path:
        current = current.children[index]
    return current


def is_number(node):
    return node.kind == "atom" and re.fullmatch(
        r"[+-]?(?:\d+|\d+\.\d*(?:[eE][+-]?\d+)?|\d+[eE][+-]?\d+)",
        node.value,
    )


def named_fields(node):
    if node.kind != "list" or len(node.children) < 3:
        return []
    fields = []
    index = 1
    while index + 1 < len(node.children):
        key = node.children[index]
        if key.kind != "atom" or not key.value.startswith(":"):
            return []
        fields.append((key.value[1:], index + 1))
        index += 2
    return fields if index == len(node.children) else []
