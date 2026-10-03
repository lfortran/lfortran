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


def is_member_name(node):
    return node.kind == "atom" and node.value.startswith(":")


def strip_member_names(node):
    """Return `node` with constructor member names dropped.

    A named constructor spells its members as `:name value` in ASDL
    declaration order, so dropping the names yields the positional form the
    committed fixtures use. Enum values are keywords too, but they sit in
    value position and are therefore kept.
    """
    clone = node.clone()
    clone.children = [strip_member_names(child) for child in clone.children]
    if clone.kind != "list" or not clone.children:
        return clone
    head = clone.children[0]
    if head.kind != "atom" or is_member_name(head):
        return clone
    rest = clone.children[1:]
    if not rest or len(rest) % 2 != 0:
        return clone
    if not all(is_member_name(rest[i]) for i in range(0, len(rest), 2)):
        return clone
    clone.children = [head] + rest[1::2]
    return clone


def render_indented(node, level=0):
    """Render `node` the way the compiler's printer does with indentation.

    Each constructor field, vector element and map entry goes on its own line
    indented by two spaces per level; an empty form, vector or map stays on
    one line, and a symbol reference is always inline. A fixture stored this
    way makes a verifier diagnostic point at one short line instead of at a
    single very long one.
    """
    pad = "  " * level
    inner = "  " * (level + 1)
    if node.kind == "atom":
        return node.value
    if node.kind == "tag":
        return f"#{node.value} {render_indented(node.children[0], level)}"
    if node.kind == "vector":
        if not node.children:
            return "[]"
        body = "".join(
            "\n" + inner + render_indented(child, level + 1)
            for child in node.children)
        return "[" + body + "\n" + pad + "]"
    if node.kind == "map":
        if not node.children:
            return "{}"
        pairs = zip(node.children[0::2], node.children[1::2])
        body = "".join(
            "\n" + inner + render_indented(key, level + 1) + " "
            + render_indented(value, level + 1)
            for key, value in pairs)
        return "{" + body + "\n" + pad + "}"
    head, rest = node.children[0], node.children[1:]
    if head.value == "SymbolRef":
        arguments = " ".join(render_indented(c, level) for c in rest)
        return f"(SymbolRef {arguments})"
    if not rest:
        return f"({head.value})"
    body = "".join(
        "\n" + inner + render_indented(child, level + 1) for child in rest)
    return "(" + head.value + body + "\n" + pad + ")"


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
