#!/usr/bin/env python3

import argparse
import json
import os
import re
import sys
import traceback
from abc import ABC, abstractmethod
from collections import defaultdict, deque
from contextlib import contextmanager
from copy import deepcopy
from dataclasses import dataclass, field
from functools import wraps
from itertools import chain
from pathlib import Path
from typing import (
  Any,
  Callable,
  Collection,
  Deque,
  Dict,
  Iterator,
  List,
  Optional,
  Self,
  Set,
  Tuple,
  Union,
)

EXIT_SUCCESS: int = 0
EXIT_FAILURE: int = 1

RE_META_MODEL_SCHEMA_REF_PART: re.Pattern = re.compile(r"[^#/]+")
RE_CAMEL_CASE_TO_UNDERSCORE: re.Pattern = \
  re.compile("(?:([a-z])([A-Z])|([A-Z0-9])([A-Z][a-z]))")
RE_UPPER_FIRST: re.Pattern = \
  re.compile("^(?:([A-Z])([^A-Z])|([A-Z]+?)([A-Z][^A-Z]|$))")

DEFAULT_INDENT_PATTERN: str = "  "

def is_debug_enabled() -> bool:
  debug: str = os.environ.get("DEBUG_LSP_CODE_GENERATOR", "false")
  return debug.lower() in ["true", "t", "yes", "y", "on", "1"]

def match_pair_to_underscore(match: re.Match) -> str:
  if match.group(1):
    lhs = match.group(1)
    rhs = match.group(2)
  else:
    lhs = match.group(3)
    rhs = match.group(4)
  return f"{lhs}_{rhs}"

def memoize(fn):
    memo = {}

    @wraps(fn)
    def wrapper(*args):
        if args in memo:
            return memo[args]
        value = fn(*args)
        memo[args] = value
        return value

    return wrapper

@memoize
def camel_case_to_underscore(source: str) -> str:
  target = RE_CAMEL_CASE_TO_UNDERSCORE.sub(
    match_pair_to_underscore,
    source
  )

  while source != target:
    source = target
    target = RE_CAMEL_CASE_TO_UNDERSCORE.sub(
      match_pair_to_underscore,
      source
    )

  return target.upper()

def match_pair_to_lower_first(match: re.Match) -> str:
  if match.group(1):
    lhs = match.group(1)
    rhs = match.group(2)
  else:
    lhs = match.group(3)
    rhs = match.group(4)
  return f"{lhs.lower()}{rhs}"

@memoize
def lower_first(name: str) -> str:
  if len(name) > 0:
    return RE_UPPER_FIRST.sub(
      match_pair_to_lower_first,
      name
    )
  return ""

@memoize
def upper_first_char(name: str) -> str:
  if name == "uinteger":
    return "UInteger"
  if len(name) > 0:
    return name[0].upper() + name[1:]
  return ""

@memoize
def method_to_camel_case(method: str) -> str:
  if method.startswith("$"):
    method = method[1:]
  return "".join(map(upper_first_char, method.split("/")))

@memoize
def method_to_underscore(method: str) -> str:
  if method.startswith("$"):
    method = method[1:]
  camel_case = method_to_camel_case(method)
  return camel_case_to_underscore(camel_case)

RESERVED_NAMES: Set[str] = {
  "any",
  "array",
  "boolean",
  "decimal",
  "integer",
  "null",
  "object",
  "string",
  "uinteger",
}

@memoize
def rename_enum(old_name: str) -> str:
  match old_name:
    case "LSPAny":
      return rename_enum("any")
    case "LSPObject":
      return rename_enum("object")
    case "LSPArray":
      return rename_enum("array")
    case _:
      new_name = camel_case_to_underscore(old_name)
      if new_name.lower() in RESERVED_NAMES:
        new_name = f"{new_name}_TYPE"
      return new_name

@memoize
def rename_type(old_name: str) -> str:
  new_name = old_name
  if new_name.lower() in RESERVED_NAMES:
    new_name = f"{new_name}_t"
  return new_name

@memoize
def any_enum(type_name: str) -> str:
  match type_name:
    case "object" | "array" | "string" | "integer" | "uinteger" | "decimal" | "boolean" | "null":
      return rename_enum(type_name)
    case "URI" | "DocumentUri" | "RegExp":
      return rename_enum("string")
    case _:
      return rename_enum("object")

@memoize
def send_fn(method: str) -> str:
  if method.startswith("$/"):
    method = method[2:]
  fn_nym = method.replace(r'/', '_')
  fn_nym = upper_first_char(fn_nym)
  return f"send{fn_nym}"

@memoize
def receive_fn(method: str) -> str:
  if method.startswith("$/"):
    method = method[2:]
  fn_nym = method.replace(r'/', '_')
  fn_nym = upper_first_char(fn_nym)
  return f"receive{fn_nym}"

def as_enumeration_spec(type_name: str, enumeration: List[Tuple[str, str]]) -> Dict[str, Any]:
  return {
    "name": type_name,
    "type": {
      "kind": "base",
      "name": "string",
    },
    "values": [
      {
        "name": enum_name,
        "value": enum_value,
      }
      for enum_name, enum_value in enumeration
    ],
  }

DEFAULT_SCHEMA: Dict[str, Any] = {
  "enumerations": [
  ],
  "structures": [
    {
      "name": "Message",
      "properties": [
        {
          "name": "jsonrpc",
          "type": {
            "kind": "base",
            "name": "string",
          },
        },
      ],
      "documentation": 'A general message as defined by JSON-RPC. The language server protocol\nalways uses “2.0” as the jsonrpc version.',
    },
    {
      "name": "RequestMessage",
      "properties": [
        {
          "name": "id",
          "type": {
            "kind": "reference",
            "name": "RequestId",
          },
          "documentation": "The request id.",
        },
        {
          "name": "method",
          "type": {
            "kind": "base",
            "name": "string",
          },
          "documentation": "The method to be invoked.",
        },
        {
          "name": "params",
          "type": {
            "kind": "reference",
            "name": "MessageParams",
          },
          "documentation": "The method's params.",
          "optional": True,
        },
      ],
      "extends": [
        {
          "kind": "reference",
          "name": "Message",
        }
      ],
      "documentation": "A request message to describe a request between the client and the server.\nEvery processed request must send a response back to the sender of the\nrequest.",
    },
    {
      "name": "NotificationMessage",
      "properties": [
        {
          "name": "method",
          "type": {
            "kind": "base",
            "name": "string",
          },
          "documentation": "The method to be invoked.",
        },
        {
          "name": "params",
          "type": {
            "kind": "reference",
            "name": "MessageParams",
          },
          "documentation": "The notification's params.",
          "optional": True,
        }
      ],
      "extends": [
        {
          "kind": "reference",
          "name": "Message",
        }
      ]
    },
    {
      "name": "ResponseError",
      "properties": [
        {
          "name": "code",
          "type": {
            "kind": "base",
            "name": "integer",
          },
          "documentation": "A number indicating the error type that occurred.",
        },
        {
          "name": "message",
          "type": {
            "kind": "base",
            "name": "string",
          },
          "documentation": "A string providing a short description of the error.",
        },
        {
          "name": "data",
          "type": {
            "kind": "reference",
            "name": "LSPAny",
          },
          "documentation": "A primitive or structured value that contains additional information about\nthe error. Can be omitted.",
          "optional": True,
        },
      ],
    },
    {
      "name": "ResponseMessage",
      "properties": [
        {
          "name": "id",
          "type": {
            "kind": "reference",
            "name": "ResponseId",
          },
          "documentation": "The request id.",
        },
        {
          "name": "result",
          "type": {
            "kind": "reference",
            "name": "LSPAny",
          },
          "documentation": "The result of a request. This member is REQUIRED on success. This member\nMUST NOT exist if there was an error invoking the method.",
          "optional": True,
        },
        {
          "name": "error",
          "type": {
            "kind": "reference",
            "name": "ResponseError",
          },
          "documentation": "The error object in case a request fails.",
          "optional": True,
        },
      ],
      "extends": [
        {
          "kind": "reference",
          "name": "Message",
        },
      ],
      "documentation": "A Response Message sent as a result of a request. If a request doesn’t\nprovide a result value the receiver of a request still needs to return a\nresponse message to conform to the JSON-RPC specification. The result\nproperty of the ResponseMessage should be set to null in this case to signal\na successful request.",
    },
  ],
  "typeAliases": [
    {
      "name": "integer",
      "type": {
        "kind": "base",
        "name": "int",
      },
    },
    {
      "name": "uinteger",
      "type": {
        "kind": "base",
        "name": "unsigned int",
      },
    },
    {
      "name": "decimal",
      "type": {
        "kind": "base",
        "name": "double",
      },
    },
    {
      "name": "boolean",
      "type": {
        "kind": "base",
        "name": "bool",
      },
    },
    {
      "name": "null",
      "type": {
        "kind": "base",
        "name": "std::nullptr_t",
      },
    },
    {
      "name": "string",
      "type": {
        "kind": "base",
        "name": "std::string",
      },
    },
    {
      "name": "URI",
      "type": {
        "kind": "base",
        "name": "string",
      },
    },
    {
      "name": "DocumentUri",
      "type": {
        "kind": "base",
        "name": "string",
      },
    },
    {
      "name": "RegExp",
      "type": {
        "kind": "base",
        "name": "string",
      },
    },
    {
      "name": "RequestId",
      "type": {
        "kind": "or",
        "items": [
          {
            "kind": "base",
            "name": "integer",
          },
          {
            "kind": "base",
            "name": "string",
          },
        ],
      },
    },
    {
      "name": "MessageParams",
      "type": {
        "kind": "or",
        "items": [
          {
            "kind": "reference",
            "name": "LSPArray",
          },
          {
            "kind": "reference",
            "name": "LSPObject",
          },
        ],
      },
      "documentation": "A request message to describe a request between the client and the server.\nEvery processed request must send a response back to the sender of the\nrequest.",
    },
    {
      "name": "ResponseId",
      "type": {
        "kind": "or",
        "items": [
          {
            "kind": "base",
            "name": "integer",
          },
          {
            "kind": "base",
            "name": "string",
          },
          {
            "kind": "base",
            "name": "null",
          },
        ],
      },
    },
  ],
  "requests": [
  ],
  "notifications": [
  ],
}

class LspCodeGenerator(ABC):
  args: argparse.Namespace
  schema: Dict[str, Any]

  def __init__(self: Self, args: argparse.Namespace) -> None:
    self.args = args

  def generate_code(self: Self) -> None:
    print('Creating parent directories')
    self.args.output_dir.mkdir(parents=True, exist_ok=True)
    print('Loading schema')
    self.schema = json.loads(self.args.schema.read())
    print('Generating files')
    self.generate_specification()
    self.generate_transformer()
    self.generate_server()
    print('Done.')

  @abstractmethod
  def generate_specification(self: Self) -> None:
    raise NotImplementedError()

  @abstractmethod
  def generate_transformer(self: Self) -> None:
    raise NotImplementedError()

  @abstractmethod
  def generate_server(self: Self) -> None:
    raise NotImplementedError()

class FileGenerator(ABC):
  file_path: Path
  schema: Dict[str, Any]

  indent_level: int = 0
  indent_pattern: str = DEFAULT_INDENT_PATTERN
  nested_names: Deque[str]

  def __init__(self: Self, file_path: Path, schema: Dict[str, Any]) -> None:
    self.file_path = file_path
    self.schema = schema
    self.nested_names = deque()

  @contextmanager
  def indent(self: Self, num_levels: int = 1) -> Iterator[Self]:
    self.indent_level += num_levels
    yield self
    self.indent_level -= num_levels

  @contextmanager
  def nest_name(self: Self, name: str) -> Iterator[str]:
    self.nested_names.append(name)
    yield self.nested_name()
    self.nested_names.pop()

  @contextmanager
  def nested_names_as(self: Self, next_nested_names: Deque[str]) -> Iterator[str]:
    prev_nested_names = self.nested_names
    self.nested_names = next_nested_names
    yield self.nested_name()
    self.nested_names = prev_nested_names

  def nested_name(self: Self, nested_names: Optional[Collection[str]] = None) -> str:
    if nested_names is None:
      nested_names = self.nested_names
    return "_".join(nested_names)

  def __enter__(self):
    return self.open()

  def __exit__(self, exc_type, exc_val, exc_tb):
    self.close()

  def open(self: Self) -> Self:
    self.file_handle = self.file_path.open("w")
    return self

  def close(self: Self) -> Self:
    self.file_handle.close()

  def inline(
      self: Self,
      message: Optional[str] = None,
      end: Optional[str] = None,
      indent: bool = False
  ) -> None:
    if indent:
      self.file_handle.write(self.indent_level * self.indent_pattern)
    if message is not None:
      self.file_handle.write(message)
    if end is not None:
      self.file_handle.write(end)

  def write(self: Self, message: str, end: Optional[str] = "\n", indent: bool = True) -> None:
    self.inline(message=message, end=end, indent=indent)

  def newline(self: Self) -> None:
    self.file_handle.write("\n")

  @abstractmethod
  def generate_code(self: Self) -> None:
    raise NotImplementedError()

class CPlusPlusFileGenerator(FileGenerator):
  namespace: str
  symbols: Dict[str, Tuple[str, Dict[str, Any]]]

  def __init__(
      self: Self,
      output_path: Path,
      schema: Dict[str, Any],
      namespace: str,
      symbols: Dict[str, Tuple[str, Dict[str, Any]]]
  ) -> None:
    super().__init__(output_path, schema)
    self.namespace = namespace
    self.symbols = symbols

  def generate_docstring(self: Self, docs: Optional[str]) -> None:
    if docs is not None:
      docs = docs.replace(r'/*', '/​*')
      self.write( '/**')
      for line in docs.split('\n'):
        self.inline(' *', indent=True)
        if len(line) > 0:
          self.inline(f' {line}')
        self.newline()
      self.write( ' */')

  def generate_variant_enumeration(
      self: Self,
      spec: Dict[str, Any]
  ) -> None:
    match spec["kind"]:
      case "base" | "reference":
        self.inline(rename_enum(spec["name"]))
      case "stringLiteral":
        self.inline(rename_enum("string"))
      case "integerLiteral":
        self.inline(rename_enum("integer"))
      case "booleanLiteral":
        self.inline(rename_enum("boolean"))
      case "array":
        self.generate_variant_enumeration(spec["element"])
        self.inline("_ARRAY");
      case "map":
        self.generate_variant_enumeration(spec["key"])
        self.generate_variant_enumeration(spec["value"])
      case "and":
        raise ValueError(
          f'AND types are not supported for type declarations: {spec}'
        )
      case "or":
        for index, item_spec in enumerate(spec["items"]):
          with self.nest_name(str(index)):
            if index > 0:
              self.inline('_OR_')
            self.generate_variant_enumeration(item_spec)
      case "tuple":
        item_specs = spec["items"]
        if len(item_specs) == 2:
          self.inline('PAIR_OF_')
        else:
          self.inline('PAIR_OF_')
        for index, item_spec in enumerate(item_specs):
          if index > 0:
            self.inline('_AND_')
          self.generate_variant_enumeration(item_spec)
      case "literal":
        self.inline(rename_enum(self.nested_name()))
      case _:
        raise ValueError(f'Unsupported variant enumeration kind ({spec["kind"]}): {spec}')

  def generate_type_declaration(self: Self, type_spec: Dict[str, Any]) -> None:
    match type_spec["kind"]:
      case "base":
        type_name = type_spec["name"]
        self.inline(rename_type(type_name))
      case "reference":
        type_name = type_spec["name"]
        match type_name:
          case "LSPObject" | "LSPArray":
            self.inline(type_name)
          case "LSPAny":
            self.inline(f'std::unique_ptr<{type_name}>')
          case _:
            symbol_kind, symbol_spec = self.symbols[type_name]
            if symbol_kind == "alias":
              symbol_type = symbol_spec["type"]
              match symbol_type["kind"]:
                case "base" | "array":
                  self.inline(type_name)
                case "reference":
                  self.inline(f'std::unique_ptr<{type_name}>')
                case _:
                  raise ValueError(f'Unsupported alias type ({symbol_type["kind"]}): {symbol_spec}')
            elif symbol_kind == "structure":
              self.inline(f'std::unique_ptr<{type_name}>')
            else:
              self.inline(type_name)
      case "array":
        self.inline('std::vector<')
        self.generate_type_declaration(type_spec["element"])
        self.inline('>')
      case "map":
        self.inline('std::map<')
        self.generate_type_declaration(type_spec["key"])
        self.inline(', ')
        self.generate_type_declaration(type_spec["value"])
        self.inline('>')
      case "and":
        raise ValueError(f'AND types are not supported for type declarations: {type_spec}')
      case "or":
        self.inline(self.nested_name())
      case "tuple":
        item_specs = type_spec["items"]
        if len(item_specs) == 2:
          self.inline('std::pair<')
        else:
          self.inline('std::tuple<')
        k = len(item_specs)
        if k > 0:
          self.generate_type_declaration(item_specs[0])
          if k > 1:
            for i in range(1, k):
              self.inline(', ')
              self.generate_type_declaration(item_specs[i])
        self.inline('>')
      case "literal":
        self.inline(f'std::unique_ptr<{self.nested_name()}>')
      case "stringLiteral":
        self.inline(rename_type('string'))
      case "integerLiteral":
        self.inline(rename_type('integer'))
      case "booleanLiteral":
        self.inline(rename_type('boolean'))
      case _:
        raise ValueError(f'Unsupported Type kind: {type_spec}')

class CPlusPlusSpecificationHeaderGenerator(CPlusPlusFileGenerator):
  generated: Set[str]
  generators: Dict[str, Callable[[Dict[str, Any]], None]]
  dependencies: Dict[str, Set[str]]
  dependents: Dict[str, Set[str]]

  def __init__(
      self: Self,
      output_dir: Path,
      schema: Dict[str, Any],
      namespace: str,
      symbols: Dict[str, Tuple[str, Dict[str, Any]]]
  ) -> None:
    specification_header = output_dir / "specification.h"
    super().__init__(specification_header, schema, namespace, symbols)
    self.generated = set()
    self.generators = {}
    self.dependencies = {}
    self.dependents = {}

  def extract_string_dependencies(
      self: Self,
      dependencies: Set[str],
      base_spec: Dict[str, Any]
  ) -> None:
    type_name = "string"
    if type_name not in self.generated:
      raise ValueError(f'Base type not generated: {type_name}')

  def extract_integer_dependencies(
      self: Self,
      dependencies: Set[str],
      base_spec: Dict[str, Any]
  ) -> None:
    type_name = "integer"
    if type_name not in self.generated:
      raise ValueError(f'Base type not generated: {type_name}')

  def extract_boolean_dependencies(
      self: Self,
      dependencies: Set[str],
      base_spec: Dict[str, Any]
  ) -> None:
    type_name = "boolean"
    if type_name not in self.generated:
      raise ValueError(f'Base type not generated: {type_name}')

  def extract_base_dependencies(
      self: Self,
      dependencies: Set[str],
      base_spec: Dict[str, Any]
  ) -> None:
    type_name = base_spec["name"]
    if type_name not in self.generated:
      raise ValueError(f'Base type not generated: {type_name}')

  def extract_reference_dependencies(
      self: Self,
      dependencies: Set[str],
      ref_spec: Dict[str, Any]
  ) -> None:
    type_name = ref_spec["name"]
    if type_name not in self.generated:
      dependencies.add(type_name)

  def extract_array_dependencies(
      self: Self,
      dependencies: Set[str],
      array_spec: Dict[str, Any]
  ) -> None:
    self.extract_type_dependencies(dependencies, array_spec["element"])

  def extract_tuple_dependencies(
      self: Self,
      dependencies: Set[str],
      tuple_spec: Dict[str, Any]
  ) -> None:
    item_specs = tuple_spec["items"]
    for item_spec in item_specs:
      self.extract_type_dependencies(dependencies, item_spec)

  def extract_map_dependencies(
      self: Self,
      dependencies: Set[str],
      map_spec: Dict[str, Any]
  ) -> None:
    key_spec = map_spec["key"]
    match key_spec["kind"]:
      case "base":
        self.extract_base_dependencies(dependencies, key_spec)
      case "reference":
        self.extract_reference_dependencies(dependencies, key_spec)
      case _:
        raise ValueError(f'Unsupported map-key type: {map_key}')
    value_spec = map_spec["value"]
    self.extract_type_dependencies(dependencies, value_spec)

  def extract_and_dependencies(
      self: Self,
      dependencies: Set[str],
      and_spec: Dict[str, Any]
  ) -> None:
    for item_spec in and_spec["items"]:
      self.extract_type_dependencies(dependencies, item_spec)

  def extract_or_dependencies(
      self: Self,
      dependencies: Set[str],
      or_spec: Dict[str, Any]
  ) -> None:
    for item_spec in or_spec["items"]:
      self.extract_type_dependencies(dependencies, item_spec)

  def extract_structure_literal_dependencies(
      self: Self,
      dependencies: Set[str],
      nested_spec: Dict[str, Any]
  ) -> None:
    for prop_spec in nested_spec["value"]["properties"]:
      self.extract_property_dependencies(dependencies, prop_spec)

  def extract_type_dependencies(
      self: Self,
      dependencies: Set[str],
      type_spec: Dict[str, Any]
  ) -> None:
    match type_spec["kind"]:
      case "base":
        self.extract_base_dependencies(dependencies, type_spec)
      case "reference":
        self.extract_reference_dependencies(dependencies, type_spec)
      case "array":
        self.extract_array_dependencies(dependencies, type_spec)
      case "map":
        self.extract_map_dependencies(dependencies, type_spec)
      case "and":
        self.extract_and_dependencies(dependencies, type_spec)
      case "or":
        self.extract_or_dependencies(dependencies, type_spec)
      case "tuple":
        self.extract_tuple_dependencies(dependencies, type_spec)
      case "literal":
        self.extract_structure_literal_dependencies(dependencies, type_spec)
      case "stringLiteral":
        self.extract_string_dependencies(dependencies, type_spec)
      case "integerLiteral":
        self.extract_integer_dependencies(dependencies, type_spec)
      case "booleanLiteral":
        self.extract_boolean_dependencies(dependencies, type_spec)
      case _:
        raise ValueError(f'Unsupported Type kind: {type_spec}')

  def extract_property_dependencies(
      self: Self,
      dependencies: Set[str],
      prop_spec: Dict[str, Any]
  ) -> None:
    type_spec = prop_spec["type"]
    self.extract_type_dependencies(dependencies, type_spec)

  def add_pending(
      self: Self,
      name: str,
      dependencies: Set[str],
      generate_fn: Callable[[Dict[str, Any]], None]
  ) -> None:
    self.dependencies[name] = dependencies
    self.generators[name] = generate_fn
    for dependency in dependencies:
      dependency_dependents = self.dependents.get(dependency, None)
      if dependency_dependents is None:
        dependency_dependents = set()
        self.dependents[dependency] = dependency_dependents
      dependency_dependents.add(name)

  def add_generated(self: Self, name: str) -> None:
    if name not in self.generated:
      self.generated.add(name)
      dependents = self.dependents.get(name, None)
      if dependents is not None:
        for dependent_name in dependents:
          dependent_dependencies = self.dependencies[dependent_name]
          # If `name` is not in `dependent_dependencies` then it was part of a
          # cycle.
          if name in dependent_dependencies:
            dependent_dependencies.remove(name)
          if len(dependent_dependencies) == 0:
            generate_dependent = self.generators[dependent_name]
            _, dependent = self.symbols[dependent_name]
            generate_dependent(dependent)
            self.add_generated(dependent_name)
            del self.dependencies[dependent_name]

  def generate_or_add_to_pending(
      self: Self,
      name: str,
      subject: Dict[str, Any],
      dependencies: Set[str],
      generate_fn: Callable[[Dict[str, Any]], None]
  ) -> bool:
    if len(dependencies) == 0:
      generate_fn(subject)
      self.add_generated(name)
      return True
    self.add_pending(name, dependencies, generate_fn)
    return False

  def resolve_cycles(self: Self) -> None:
    pending: Deque[Tuple[str, Set[str]]] = deque(self.dependencies.items())
    closures: Dict[str, Deque[str]] = {}
    while len(pending) > 0:
      name, dependencies = pending.pop()
      closure = closures.get(name, None)
      if closure is None:
        closure = deque()
        closures[name] = closure
      for dependency_name in dependencies:
        if dependency_name not in closure:
          closure.append(dependency_name)
          dependency_dependencies = self.dependencies[dependency_name]
          pending.append((dependency_name, dependency_dependencies))
    declared: Set[str] = set()
    for name, closure in closures.items():
      while len(closure) > 0:
        dependency_name = closure.popleft()
        if dependency_name not in self.generated \
           and dependency_name not in declared:
          self.write(f'struct {dependency_name};  // forward declaration')
          self.newline()
          declared.add(dependency_name)
      self.add_generated(name)

  def generate_enumeration(self: Self, enum_spec: Dict[str, Any]) -> None:
    enum_name = enum_spec["name"]
    lower_name = lower_first(enum_name)
    type_spec = enum_spec["type"]
    type_name = type_spec["name"]
    value_type = rename_type(type_name)
    self.generate_docstring(enum_spec.get("documentation", None))
    self.write(f'enum class {enum_name} {{')
    with self.indent():
      if type_name == "string":
        for value in enum_spec["values"]:
          self.generate_docstring(value.get("documentation", None))
          self.write(f'{rename_enum(value["name"])},')
      else:
        for value in enum_spec["values"]:
          self.generate_docstring(value.get("documentation", None))
          self.write(f'{rename_enum(value["name"])} = {value["value"]},')
    self.write('};')
    self.newline()
    self.write('extern std::map<')
    with self.indent(): self.write(f'{enum_name},')
    with self.indent(): self.write('std::string')
    self.write(f'> {enum_name}Names;')
    self.newline()
    if type_name == "string":
      self.write('extern std::map<')
      with self.indent(): self.write(f'{enum_name},')
      with self.indent(): self.write(value_type)
      self.write(f'> {enum_name}Values;')
      self.newline()
    self.write(f'auto {lower_name}ByName(')
    with self.indent(): self.write('const std::string &name')
    self.write(f') -> {enum_name};')
    self.newline()
    if type_name == "string":
      self.write(f'auto {lower_name}ByValue(')
      with self.indent(): self.write(f'const {value_type} &value')
      self.write(f') -> {enum_name};')
    else:
      self.write(f'auto {lower_name}ByValue(')
      with self.indent(): self.write(f'{value_type} value')
      self.write(f') -> {enum_name};')

  def generate_enumerations(self: Self) -> None:
    enum_specs = chain(
      DEFAULT_SCHEMA["enumerations"],
      self.schema["enumerations"],
    )
    for enum_spec in enum_specs:
      enum_name = enum_spec["name"]
      self.symbols[enum_name] = ("enumeration", enum_spec)
      self.generate_enumeration(enum_spec)
      self.newline()
      self.generated.add(enum_name)

  def generate_property(self: Self, prop_spec: Dict[str, Any]) -> None:
    with self.nest_name(prop_spec["name"]):
      type_spec = prop_spec["type"]
      optional = prop_spec.get("optional", False)
      if optional:
        self.inline('std::optional<')
      self.generate_type_declaration(prop_spec["type"])
      if optional:
        self.inline('>')
      self.inline(f' {prop_spec["name"]};', end='\n')

  def extract_nested_dependencies(
      self: Self,
      nested_dependencies: Deque[
        Tuple[
          Deque[str],
          Callable[
            [
              Deque[str],
              Dict[str, Any]
            ],
            None
          ],
          Dict[str, Any]
        ]
      ],
      spec: Dict[str, Any]
  ) -> None:
    match spec["kind"]:
      case "base" | "stringLiteral" | "integerLiteral" | "booleanLiteral" | "reference":
        pass
      case "array":
        self.extract_nested_dependencies(nested_dependencies, spec["element"])
      case "map":
        self.extract_nested_dependencies(nested_dependencies, spec["key"])
        self.extract_nested_dependencies(nested_dependencies, spec["value"])
      case "and":
        raise ValueError(
          f'AND types are not supported for type declarations: {spec}'
        )
      case "or":
        nested_dependencies.append((
          deque(self.nested_names),
          self.generate_nested_variant,
          spec
        ))
        for index, item_spec in enumerate(spec["items"]):
          with self.nest_name(str(index)):
            self.extract_nested_dependencies(nested_dependencies, item_spec)
      case "tuple":
        for item_spec in spec["items"]:
          self.extract_nested_dependencies(nested_dependencies, item_spec)
      case "literal":
        nested_dependencies.append((
          deque(self.nested_names),
          self.generate_nested_structure,
          spec
        ))
        for prop_spec in spec["value"]["properties"]:
          with self.nest_name(prop_spec["name"]):
            self.extract_nested_dependencies(nested_dependencies, prop_spec["type"])
      case _:
        raise ValueError(f'Unsupported Type kind: {spec}')

  def generate_nested_variant(
      self: Self,
      nested_names: Deque[str],
      spec_or_items: Union[Dict[str, Any], List[Dict[str, Any]]]
  ) -> None:
    with self.nested_names_as(nested_names) as nested_name:
      if isinstance(spec_or_items, dict):
        spec = spec_or_items
        item_specs = spec["items"]
        self.symbols[nested_name] = ("union", spec)
      else:
        spec = None
        item_specs = spec_or_items
      self.write(f'enum class {nested_name}Type {{')
      with self.indent():
        for index, item_spec in enumerate(item_specs):
          with self.nest_name(str(index)):
            self.inline(indent=True)
            self.generate_variant_enumeration(item_spec)
            self.inline(',', end='\n')
      self.write('};')
      self.newline()
      self.write(f'extern std::map<{nested_name}Type, std::string> {nested_name}TypeNames;')
      self.newline()
      if spec is not None:
        spec_docs = spec.get("documentation", None)
        if spec_docs is None and nested_name in self.symbols:
          _, sym_spec = self.symbols[nested_name]
          spec_docs = sym_spec.get("documentation", None)
        self.generate_docstring(spec_docs)
      self.write('typedef std::variant<')
      with self.indent():
        k = len(item_specs)
        if k > 0:
          item_spec = item_specs[0]
          with self.nest_name(str(0)):
            self.inline(indent=True)
            self.generate_type_declaration(item_spec)
          if k > 1:
            for i in range(1, k):
              item_spec = item_specs[i]
              with self.nest_name(str(i)):
                self.inline(',', end='\n')
                self.inline(indent=True)
                self.generate_type_declaration(item_spec)
      self.newline()
      if nested_name != "LSPAny":
        self.write(f'> {nested_name};')
      else:
        self.write(f'> {nested_name}Base;')
        self.newline()
        self.write('struct LSPAny')
        with self.indent(): self.write(': public LSPAnyBase')
        self.write('{')
        with self.indent():
          self.write('using LSPAnyBase::variant;')
        self.write('};')
      self.newline()

  def generate_nested_structure(
      self: Self,
      nested_names: Deque[str],
      spec: Dict[str, Any]
  ) -> None:
    with self.nested_names_as(nested_names) as nested_name:
      self.symbols[nested_name] = ("literal", spec)
      self.generate_docstring(spec.get("documentation", None))
      self.write(f'struct {nested_name}')
      self.write('{')
      with self.indent():
        for prop_spec in spec["value"]["properties"]:
          self.inline(indent=True)
          self.generate_property(prop_spec)
      self.write('};')
      self.newline()

  def generate_structure(self: Self, struct_spec: Dict[str, Any]) -> None:
    with self.nested_names_as(deque([struct_spec["name"]])) as struct_name:
      nested_dependencies = deque()
      pending_prop_specs = deque()
      pending = deque([struct_spec])
      while len(pending) > 0:
        spec = pending.popleft()
        for prop_spec in reversed(spec["properties"]):
          with self.nest_name(prop_spec["name"]):
            self.extract_nested_dependencies(nested_dependencies, prop_spec["type"])
            pending_prop_specs.append(prop_spec)
        spec_mixins = spec.get("mixins", None)
        if spec_mixins is not None:
          for type_spec in spec_mixins:
            mixin_name = type_spec["name"]
            _, mixin_spec = self.symbols[mixin_name]
            pending.append(mixin_spec)
      while len(nested_dependencies) > 0:
        nested_names, generate_fn, nested_spec = nested_dependencies.pop()
        generate_fn(nested_names, nested_spec)
        self.add_generated(self.nested_name(nested_names))
      self.generate_docstring(struct_spec.get("documentation", None))
      self.write(f'struct {struct_name}')
      spec_extends = struct_spec.get("extends", None)
      if spec_extends is not None:
        with self.indent():
          super_iter = iter(spec_extends)
          super_spec = next(super_iter, None)
          if super_spec is not None:
            self.write(f': public {super_spec["name"]}')
          for super_spec in super_iter:
            self.write(f', public {super_spec["name"]}')
      self.write('{')
      with self.indent():
        while len(pending_prop_specs) > 0:
          prop_spec = pending_prop_specs.pop()
          self.generate_docstring(prop_spec.get("documentation", None))
          self.inline(indent=True)
          self.generate_property(prop_spec)
      self.write('};')
      self.newline()

  def generate_structures(self: Self) -> None:
    struct_specs = chain(
      DEFAULT_SCHEMA["structures"],
      self.schema["structures"],
    )
    for struct_spec in struct_specs:
      struct_name = struct_spec["name"]
      self.symbols[struct_name] = ("structure", struct_spec)
      dependencies = set()
      struct_extends = struct_spec.get("extends", None)
      if struct_extends is not None:
        for type_spec in struct_extends:
          self.extract_type_dependencies(dependencies, type_spec)
      struct_mixins = struct_spec.get("mixins", None)
      if struct_mixins is not None:
        for type_spec in struct_mixins:
          self.extract_type_dependencies(dependencies, type_spec)
      for prop_spec in struct_spec["properties"]:
        self.extract_property_dependencies(dependencies, prop_spec)
      self.generate_or_add_to_pending(
        struct_name,
        struct_spec,
        dependencies,
        self.generate_structure
      )

  def generate_type_alias(self: Self, alias_spec: Dict[str, Any]) -> None:
    alias_name = alias_spec["name"]
    with self.nested_names_as(deque([alias_name])):
      type_spec = alias_spec["type"]
      nested_dependencies = deque()
      self.extract_nested_dependencies(nested_dependencies, type_spec)
      while len(nested_dependencies) > 0:
        nested_names, generate_fn, nested_spec = nested_dependencies.pop()
        generate_fn(nested_names, nested_spec)
        self.add_generated(self.nested_name(nested_names))
      if alias_name not in self.generated:
        self.generate_docstring(alias_spec.get("documentation", None))
        match type_spec["kind"]:
          case "base" | "array" | "map":
            self.inline('typedef ', indent=True)
            self.generate_type_declaration(type_spec)
            self.inline(f' {alias_name};', end='\n')
            self.newline()
          case "reference":
            self.inline('typedef ', indent=True)
            self.inline(type_spec["name"])
            self.inline(f' {alias_name};', end='\n')
            self.newline()
          case "or":
            self.generate_nested_variant(self.nested_names, type_spec)
          case _:
            raise ValueError(f'Unsupported alias type ({type_spec["kind"]}): {type_spec}')

  def generate_type_aliases(self: Self) -> None:
    alias_specs = chain(
      DEFAULT_SCHEMA["typeAliases"],
      self.schema["typeAliases"],
    )
    for alias_spec in alias_specs:
      alias_name = alias_spec["name"]
      if alias_name not in self.generated:
        self.symbols[alias_name] = ("alias", alias_spec)
        dependencies = set()
        self.extract_type_dependencies(dependencies, alias_spec["type"])
        self.generate_or_add_to_pending(
          alias_name,
          alias_spec,
          dependencies,
          self.generate_type_alias
        )

  def generate_request(self: Self, request_spec: Dict[str, Any]) -> None:
    request_method = request_spec["method"]
    request_name = method_to_camel_case(request_method)
    result_name = f'{request_name}Result'
    with self.nested_names_as(deque([result_name])):
      result_spec = request_spec["result"]
      partial_result_spec = request_spec.get("partialResult", None)
      if partial_result_spec is not None:
        if result_spec["kind"] == partial_result_spec["kind"] == "or":
          result_types = list(result_spec["items"])
          for item_spec in partial_result_spec["items"]:
            if item_spec not in result_types:
              result_types.append(item_spec)
        elif result_spec["kind"] == "or":
          result_types = list(result_spec["items"])
          if partial_result_spec not in result_types:
            result_types.append(partial_result_spec)
        elif partial_result_spec["kind"] == "or":
          result_types = list(partial_result_spec["items"])
          if result_spec not in result_types:
            result_types.append(result_spec)
        else:
          result_types = [result_spec]
          if partial_result_spec not in result_types:
            result_types.append(partial_result_spec)
      elif result_spec["kind"] == "or":
        result_types = result_spec["items"]
      else:
        result_types = [result_spec]
      nested_dependencies = deque()
      if len(result_types) > 1:
        result_type = {
          "kind": "or",
          "items": result_types
        }
        self.symbols[result_name] = ("union", result_type)
        self.extract_nested_dependencies(nested_dependencies, result_type)
      else:
        result_type = result_types[0]
        self.symbols[result_name] = ("reference", result_type)
        self.extract_nested_dependencies(nested_dependencies, result_type)
      while len(nested_dependencies) > 0:
        nested_names, generate_fn, nested_spec = nested_dependencies.pop()
        generate_fn(nested_names, nested_spec)
      if len(result_types) == 1:
        if result_name not in self.generated:
          result_type = result_types[0]
          self.inline('typedef ', indent=True)
          if result_type["kind"] == "reference":
            self.inline(result_type["name"])
          else:
            self.generate_type_declaration(result_type)
          self.inline(f' {result_name};', end='\n')
          self.newline()

  def generate_requests(self: Self) -> None:
    request_specs = chain(
      DEFAULT_SCHEMA["requests"],
      self.schema["requests"],
    )
    incoming_requests = []
    outgoing_requests = []
    for request_spec in request_specs:
      request_method = request_spec["method"]
      enumeration = (method_to_underscore(request_method), request_method)
      match request_spec["messageDirection"]:
        case "clientToServer":
          incoming_requests.append(enumeration)
        case "serverToClient":
          outgoing_requests.append(enumeration)
      self.symbols[request_method] = ("request", request_spec)
      dependencies = set()
      request_params = request_spec.get("params", None)
      if request_params is not None:
        self.extract_type_dependencies(dependencies, request_params)
      self.extract_type_dependencies(dependencies, request_spec["result"])
      partial_result_type = request_spec.get("partialResult", None)
      if partial_result_type is not None:
        self.extract_type_dependencies(dependencies, partial_result_type)
      self.generate_or_add_to_pending(
        request_method,
        request_spec,
        dependencies,
        self.generate_request
      )
    incoming_requests.sort(key=lambda pair: pair[0])
    outgoing_requests.sort(key=lambda pair: pair[0])
    self.write('enum class IncomingRequest')
    self.write('{')
    with self.indent():
      for enum_name, enum_value in incoming_requests:
        self.write(f'{enum_name},')
    self.write('};')
    self.newline()
    self.write('extern std::map<IncomingRequest, std::string> IncomingRequestNames;')
    self.write('extern std::map<IncomingRequest, std::string> IncomingRequestValues;')
    self.newline()
    self.write(f'auto incomingRequestByName(const std::string &name) -> IncomingRequest;')
    self.write(f'auto incomingRequestByValue(const std::string &value) -> IncomingRequest;')
    self.write(f'auto isIncomingRequest(const std::string &value) -> bool;')
    self.newline()
    self.write('enum class OutgoingRequest')
    self.write('{')
    with self.indent():
      for enum_name, enum_value in outgoing_requests:
        self.write(f'{enum_name},')
    self.write('};')
    self.newline()
    self.write('extern std::map<OutgoingRequest, std::string> OutgoingRequestNames;')
    self.write('extern std::map<OutgoingRequest, std::string> OutgoingRequestValues;')
    self.newline()
    self.write(f'auto outgoingRequestByName(const std::string &name) -> OutgoingRequest;')
    self.write(f'auto outgoingRequestByValue(const std::string &value) -> OutgoingRequest;')
    self.newline()

  def generate_notification(self: Self, notification_spec: Dict[str, Any]) -> None:
    pass

  def generate_notifications(self: Self) -> None:
    notification_specs = chain(
      DEFAULT_SCHEMA["notifications"],
      self.schema["notifications"],
    )
    incoming_notifications = []
    outgoing_notifications = []
    for notification_spec in notification_specs:
      notification_method = notification_spec["method"]
      enumeration = (method_to_underscore(notification_method), notification_method)
      match notification_spec["messageDirection"]:
        case "clientToServer":
          incoming_notifications.append(enumeration)
        case "serverToClient":
          outgoing_notifications.append(enumeration)
      self.symbols[notification_method] = ("notification", notification_spec)
      dependencies = set()
      notification_params = notification_spec.get("params", None)
      if notification_params is not None:
        self.extract_type_dependencies(dependencies, notification_params)
      self.generate_or_add_to_pending(
        notification_method,
        notification_spec,
        dependencies,
        self.generate_notification
      )
    incoming_notifications.sort(key=lambda pair: pair[0])
    outgoing_notifications.sort(key=lambda pair: pair[0])
    self.write('enum class IncomingNotification')
    self.write('{')
    with self.indent():
      for enum_name, enum_value in incoming_notifications:
        self.write(f'{enum_name},')
    self.write('};')
    self.newline()
    self.write('extern std::map<IncomingNotification, std::string> IncomingNotificationNames;')
    self.write('extern std::map<IncomingNotification, std::string> IncomingNotificationValues;')
    self.newline()
    self.write(f'auto incomingNotificationByName(const std::string &name) -> IncomingNotification;')
    self.write(f'auto incomingNotificationByValue(const std::string &value) -> IncomingNotification;')
    self.write(f'auto isIncomingNotification(const std::string &value) -> bool;')
    self.newline()
    self.write('enum class OutgoingNotification')
    self.write('{')
    with self.indent():
      for enum_name, enum_value in outgoing_notifications:
        self.write(f'{enum_name},')
    self.write('};')
    self.newline()
    self.write('extern std::map<OutgoingNotification, std::string> OutgoingNotificationNames;')
    self.write('extern std::map<OutgoingNotification, std::string> OutgoingNotificationValues;')
    self.newline()
    self.write(f'auto outgoingNotificationByName(const std::string &name) -> OutgoingNotification;')
    self.write(f'auto outgoingNotificationByValue(const std::string &value) -> OutgoingNotification;')
    self.newline()

  def generate_lsp_any(self: Self) -> None:
    for alias_spec in self.schema["typeAliases"]:
      alias_name = alias_spec["name"]
      if alias_name == "LSPAny":
        self.symbols[alias_name] = ("alias", alias_spec)
        dependencies = set()
        self.extract_type_dependencies(dependencies, alias_spec["type"])
        self.generate_or_add_to_pending(
          alias_name,
          alias_spec,
          dependencies,
          self.generate_type_alias
        )
        break

  def generate_lsp_object(self: Self) -> None:
    for alias_spec in self.schema["typeAliases"]:
      alias_name = alias_spec["name"]
      if alias_name == "LSPObject":
        self.symbols[alias_name] = ("alias", alias_spec)
        dependencies = set()
        self.extract_type_dependencies(dependencies, alias_spec["type"])
        dependencies.remove("LSPAny")
        self.generate_or_add_to_pending(
          alias_name,
          alias_spec,
          dependencies,
          self.generate_type_alias
        )
        break

  def generate_lsp_array(self: Self) -> None:
    for alias_spec in self.schema["typeAliases"]:
      alias_name = alias_spec["name"]
      if alias_name == "LSPArray":
        self.symbols[alias_name] = ("alias", alias_spec)
        dependencies = set()
        self.extract_type_dependencies(dependencies, alias_spec["type"])
        dependencies.remove("LSPAny")
        self.generate_or_add_to_pending(
          alias_name,
          alias_spec,
          dependencies,
          self.generate_type_alias
        )
        break

  def generate_code(self: Self) -> None:
    print(f'Generating: {self.file_path}')
    version: str = self.schema["metaData"]["version"]

    lower_index = 0
    upper_index = version.index(".", lower_index)
    major_version: str = version[lower_index:upper_index]

    lower_index = upper_index + 1
    upper_index = version.index(".", lower_index)
    minor_version: str = version[lower_index:upper_index]

    lower_index = upper_index + 1
    upper_index = len(version)
    micro_version: str = version[lower_index:upper_index]

    self.write('// -----------------------------------------------------------------------------')
    self.write('// NOTE: This file was generated from Microsoft\'s Language Server Protocol (LSP)')
    self.write('// specification. Please do not edit it by hand.')
    self.write('// -----------------------------------------------------------------------------')
    self.newline()
    self.write('#pragma once')
    self.newline()
    self.write('#include <cstddef>')
    self.write('#include <map>')
    self.write('#include <memory>')
    self.write('#include <optional>')
    self.write('#include <string>')
    self.write('#include <tuple>')
    self.write('#include <utility>')
    self.write('#include <variant>')
    self.write('#include <vector>')
    self.newline()
    self.write('/**')
    self.write(f' * Interface definitions from the LSP {version} specification.')
    self.write(f' * See: https://microsoft.github.io/language-server-protocol/specifications/lsp/{major_version}.{minor_version}/specification')
    self.write(' */')
    self.write(f'namespace {self.namespace} {{')
    with self.indent():
      self.write('const std::string JSON_RPC_VERSION = "2.0";')
      self.write(f'const std::string LSP_VERSION = "{version}";')
      self.newline()
      self.write(f'typedef int {rename_type("integer")};')
      self.write(f'typedef unsigned int {rename_type("uinteger")};')
      self.write(f'typedef double {rename_type("decimal")};')
      self.write(f'typedef bool {rename_type("boolean")};')
      self.write(f'typedef std::nullptr_t {rename_type("null")};')
      self.write(f'typedef std::string {rename_type("string")};')
      self.newline()
      self.write(f'typedef {rename_type("string")} URI;')
      self.write(f'typedef {rename_type("string")} DocumentUri;')
      self.write(f'typedef {rename_type("string")} RegExp;')

      self.generated.update([
        "bool",
        "boolean",
        "decimal",
        "DocumentUri",
        "double",
        "int",
        "integer",
        "null",
        "RegExp",
        "std::nullptr_t",
        "std::string",
        "string",
        "uinteger",
        "unsigned int",
        "URI",
      ])

      self.newline()
      self.write('struct LSPAny;  // Forward declaration')
      self.newline()
      self.generate_lsp_object()
      self.generate_lsp_array()
      self.generate_lsp_any()
      self.generate_enumerations()
      self.generate_structures()
      self.generate_type_aliases()
      self.generate_requests()
      self.generate_notifications()
      self.resolve_cycles()
      if len(self.dependencies) > 0:
        raise RuntimeError(f'There are {len(self.dependencies)} symbols remaining to be generated.')

    self.write(f'}} // namespace {self.namespace}')

class CPlusPlusSpecificationSourceGenerator(CPlusPlusFileGenerator):
  generated: Set[str]

  def __init__(
      self: Self,
      output_dir: Path,
      schema: Dict[str, Any],
      namespace: str,
      symbols: Dict[str, Tuple[str, Dict[str, Any]]]
  ) -> None:
    specification_source = output_dir / "specification.cpp"
    super().__init__(specification_source, schema, namespace, symbols)
    self.generated = set()

  def generate_enumeration(self: Self, enum_spec: Dict[str, Any]) -> None:
    enum_name = enum_spec["name"]
    lower_name = lower_first(enum_name)
    value_type = rename_type(enum_spec["type"]["name"])
    self.write('std::map<')
    with self.indent(): self.write(f'{enum_name},')
    with self.indent(): self.write('std::string')
    self.write(f'> {enum_name}Names = {{')
    with self.indent():
        for value in enum_spec["values"]:
          value_name = rename_enum(value["name"])
          self.write(f'{{{enum_name}::{value_name}, "{value_name}"}},')
    self.write('};')
    self.newline()
    if value_type == rename_type("string"):
      self.write('std::map<')
      with self.indent(): self.write(f'{enum_name},')
      with self.indent(): self.write(value_type)
      self.write(f'> {enum_name}Values = {{')
      with self.indent():
        match value_type:
          case "string_t":
            for value in enum_spec["values"]:
              value_name = rename_enum(value["name"])
              self.write(f'{{{enum_name}::{value_name}, "{value["value"]}"}},')
          case _:
            for value in enum_spec["values"]:
              value_name = rename_enum(value["name"])
              self.write(f'{{{enum_name}::{value_name}, {value["value"]}}},')
      self.write('};')
      self.newline()
    self.write(f'auto {lower_name}ByName(')
    with self.indent(): self.write('const std::string &name')
    self.write(f') -> {enum_name} {{')
    with self.indent():
      self.write(f'for (const auto &[enum_name, enum_value]')
      with self.indent(2): self.write(f': {enum_name}Names) {{')
      with self.indent():
        self.write('if (name == enum_value) {')
        with self.indent(): self.write('return enum_name;')
        self.write('}')
      self.write('}')
      self.write('throw std::invalid_argument(')
      with self.indent():
        self.write(f'"Invalid {enum_name} name: " + name')
      self.write(');')
    self.write('}')
    self.newline()
    if value_type == rename_type("string"):
      self.write(f'auto {lower_name}ByValue(')
      with self.indent(): self.write(f'const {value_type} &value')
      self.write(f') -> {enum_name} {{')
      with self.indent():
        self.write(f'for (const auto &[enum_name, enum_value]')
        with self.indent(2): self.write(f': {enum_name}Values) {{')
        with self.indent():
          self.write('if (value == enum_value) {')
          with self.indent(): self.write('return enum_name;')
          self.write('}')
        self.write('}')
        self.write('throw std::invalid_argument(')
        with self.indent():
          self.write(f'"Invalid {enum_name} value: " + value')
        self.write(');')
      self.write('}')
    else:
      self.write(f'auto {lower_name}ByValue(')
      with self.indent(): self.write(f'{value_type} value')
      self.write(f') -> {enum_name} {{')
      with self.indent():
        self.write(f'for (const auto &[field_name, field_value]')
        with self.indent(2): self.write(f': {enum_name}Names) {{')
        with self.indent():
          self.write(f'if (value == static_cast<{value_type}>(field_name)) {{')
          with self.indent(): self.write('return field_name;')
          self.write('}')
        self.write('}')
        self.write('throw std::invalid_argument(')
        with self.indent():
          self.write(f'"Invalid {enum_name} value: " + value')
        self.write(');')
      self.write('}')
    self.newline()

  def generate_enumerations(self: Self) -> None:
    enum_specs = chain(
      DEFAULT_SCHEMA["enumerations"],
      self.schema["enumerations"],
    )
    for enum_spec in enum_specs:
      self.generate_enumeration(enum_spec)

  def generate_structure(self: Self, struct_spec: Dict[str, Any]) -> None:
    with self.nested_names_as(deque([struct_spec["name"]])) as struct_name:
      nested_dependencies = deque()
      pending = deque([struct_spec])
      while len(pending) > 0:
        spec = pending.popleft()
        for prop_spec in reversed(spec["properties"]):
          with self.nest_name(prop_spec["name"]):
            self.extract_nested_dependencies(nested_dependencies, prop_spec["type"])
        spec_mixins = spec.get("mixins", None)
        if spec_mixins is not None:
          for type_spec in spec_mixins:
            mixin_name = type_spec["name"]
            _, mixin_spec = self.symbols[mixin_name]
            pending.append(mixin_spec)
      while len(nested_dependencies) > 0:
        nested_names, generate_fn, nested_spec = nested_dependencies.pop()
        generate_fn(nested_names, nested_spec)
        self.generated.add(self.nested_name(nested_names))

  def generate_structures(self: Self) -> None:
    struct_specs = chain(
      DEFAULT_SCHEMA["structures"],
      self.schema["structures"],
    )
    for struct_spec in struct_specs:
      self.generate_structure(struct_spec)

  def generate_nested_variant(
      self: Self,
      nested_names: Deque[str],
      spec_or_items: Union[Dict[str, Any], List[Dict[str, Any]]]
  ) -> None:
    with self.nested_names_as(nested_names) as nested_name:
      if isinstance(spec_or_items, dict):
        spec = spec_or_items
        item_specs = spec["items"]
        self.symbols[nested_name] = ("union", spec)
      else:
        spec = None
        item_specs = spec_or_items
      self.write(f'std::map<{nested_name}Type, std::string> {nested_name}TypeNames = {{')
      with self.indent():
        for index, item_spec in enumerate(item_specs):
          with self.nest_name(str(index)):
            self.inline(f'{{{nested_name}Type::', indent=True)
            self.generate_variant_enumeration(item_spec)
            self.inline(', "')
            self.generate_variant_enumeration(item_spec)
            self.inline('"},', end='\n')
      self.write('};')
      self.newline()

  def generate_nested_structure(
      self: Self,
      nested_names: Deque[str],
      spec: Dict[str, Any]
  ) -> None:
    with self.nested_names_as(nested_names) as nested_name:
      pass

  def extract_nested_dependencies(
      self: Self,
      nested_dependencies: Deque[
        Tuple[
          Deque[str],
          Callable[
            [
              Deque[str],
              Dict[str, Any]
            ],
            None
          ],
          Dict[str, Any]
        ]
      ],
      spec: Dict[str, Any]
  ) -> None:
    match spec["kind"]:
      case "base" | "stringLiteral" | "integerLiteral" | "booleanLiteral" | "reference":
        pass
      case "array":
        self.extract_nested_dependencies(nested_dependencies, spec["element"])
      case "map":
        self.extract_nested_dependencies(nested_dependencies, spec["key"])
        self.extract_nested_dependencies(nested_dependencies, spec["value"])
      case "and":
        raise ValueError(
          f'AND types are not supported for type declarations: {spec}'
        )
      case "or":
        nested_dependencies.append((
          deque(self.nested_names),
          self.generate_nested_variant,
          spec
        ))
        for index, item_spec in enumerate(spec["items"]):
          with self.nest_name(str(index)):
            self.extract_nested_dependencies(nested_dependencies, item_spec)
      case "tuple":
        for item_spec in spec["items"]:
          self.extract_nested_dependencies(nested_dependencies, item_spec)
      case "literal":
        nested_dependencies.append((
          deque(self.nested_names),
          self.generate_nested_structure,
          spec
        ))
        for prop_spec in spec["value"]["properties"]:
          with self.nest_name(prop_spec["name"]):
            self.extract_nested_dependencies(nested_dependencies, prop_spec["type"])
      case _:
        raise ValueError(f'Unsupported Type kind: {spec}')

  def generate_type_alias(self: Self, alias_spec: Dict[str, Any]) -> None:
    alias_name = alias_spec["name"]
    with self.nested_names_as(deque([alias_name])):
      type_spec = alias_spec["type"]
      nested_dependencies = deque()
      self.extract_nested_dependencies(nested_dependencies, type_spec)
      while len(nested_dependencies) > 0:
        nested_names, generate_fn, nested_spec = nested_dependencies.pop()
        generate_fn(nested_names, nested_spec)
        self.generated.add(self.nested_name(nested_names))
      if alias_name not in self.generated:
        match type_spec["kind"]:
          case "base" | "array" | "map" | "reference":
            pass
          case "or":
            self.generate_nested_variant(self.nested_names, type_spec)
          case _:
            raise ValueError(f'Unsupported alias type ({type_spec["kind"]}): {type_spec}')

  def generate_type_aliases(self: Self) -> None:
    alias_specs = chain(
      DEFAULT_SCHEMA["typeAliases"],
      self.schema["typeAliases"],
    )
    for alias_spec in alias_specs:
      self.generate_type_alias(alias_spec)

  def generate_request(self: Self, request_spec: Dict[str, Any]) -> None:
    pass

  def generate_is_incoming_request(self: Self) -> None:
    self.write('auto isIncomingRequest(const std::string &value) -> bool {')
    with self.indent():
      self.write('for (const auto &[enum_key, enum_value] : IncomingRequestValues) {')
      with self.indent():
        self.write('if (value == enum_value) {')
        with self.indent():
          self.write('return true;')
        self.write('}')
      self.write('}')
      self.write('return false;')
    self.write('}')
    self.newline()

  def generate_requests(self: Self) -> None:
    request_specs = chain(
      DEFAULT_SCHEMA["requests"],
      self.schema["requests"],
    )
    incoming_requests = []
    outgoing_requests = []
    for request_spec in request_specs:
      request_method = request_spec["method"]
      enumeration = (method_to_underscore(request_method), request_method)
      match request_spec["messageDirection"]:
        case "clientToServer":
          incoming_requests.append(enumeration)
        case "serverToClient":
          outgoing_requests.append(enumeration)
      self.generate_request(request_spec)
    self.generate_enumeration(
      as_enumeration_spec("IncomingRequest", incoming_requests)
    )
    self.generate_is_incoming_request()
    self.generate_enumeration(
      as_enumeration_spec("OutgoingRequest", outgoing_requests)
    )

  def generate_notification(self: Self, notification_spec: Dict[str, Any]) -> None:
    pass

  def generate_is_incoming_notification(self: Self) -> None:
    self.write('auto isIncomingNotification(const std::string &value) -> bool {')
    with self.indent():
      self.write('for (const auto &[enum_key, enum_value] : IncomingNotificationValues) {')
      with self.indent():
        self.write('if (value == enum_value) {')
        with self.indent():
          self.write('return true;')
        self.write('}')
      self.write('}')
      self.write('return false;')
    self.write('}')
    self.newline()

  def generate_notifications(self: Self) -> None:
    notification_specs = chain(
      DEFAULT_SCHEMA["notifications"],
      self.schema["notifications"],
    )
    incoming_notifications = []
    outgoing_notifications = []
    for notification_spec in notification_specs:
      notification_method = notification_spec["method"]
      enumeration = (method_to_underscore(notification_method), notification_method)
      match notification_spec["messageDirection"]:
        case "clientToServer":
          incoming_notifications.append(enumeration)
        case "serverToClient":
          outgoing_notifications.append(enumeration)
      self.generate_notification(notification_spec)
    self.generate_enumeration(
      as_enumeration_spec("IncomingNotification", incoming_notifications)
    )
    self.generate_is_incoming_notification()
    self.generate_enumeration(
      as_enumeration_spec("OutgoingNotification", outgoing_notifications)
    )

  def generate_code(self: Self) -> None:
    print(f'Generating: {self.file_path}')
    self.write('// -----------------------------------------------------------------------------')
    self.write('// NOTE: This file was generated from Microsoft\'s Language Server Protocol (LSP)')
    self.write('// specification. Please do not edit it by hand.')
    self.write('// -----------------------------------------------------------------------------')
    self.newline()
    self.write('#include <stdexcept>')
    self.newline()
    self.write('#include <server/specification.h>')
    self.newline()
    self.write(f'namespace {self.namespace} {{')
    with self.indent():
      self.generate_enumerations()
      self.generate_structures()
      self.generate_type_aliases()
      self.generate_requests()
      self.generate_notifications()
    self.write(f'}} // namespace {self.namespace}')

class CPlusPlusLspTransformerHeaderGenerator(CPlusPlusFileGenerator):
  generated_to_any: Set[str]

  def __init__(
      self: Self,
      output_dir: Path,
      schema: Dict[str, Any],
      namespace: str,
      symbols: Dict[str, Tuple[str, Dict[str, Any]]]
  ) -> None:
    specification_source = output_dir / "lsp_transformer.h"
    super().__init__(specification_source, schema, namespace, symbols)
    self.generated_to_any = set()

  def generate_as_message_params_type(self: Self, type_spec: Dict[str, Any]) -> None:
    match type_spec["kind"]:
      case "reference":
        self.inline(f'std::unique_ptr<{type_spec["name"]}>')
      # case "array":
      #   self.inline('std::vector<')
      #   self.generate_request_params_type(spec["element"])
      #   self.inline('>')
      case _:
        raise ValueError(f'Unsupported request parameter type: {type_spec}')

  def generate_as_incoming_params(
      self: Self,
      params_name: str,
      message_spec: Dict[str, Any]
  ) -> None:
    message_name = method_to_camel_case(message_spec["method"])
    params_spec = message_spec.get("params", None)
    if params_spec is not None:
      self.write(f'auto as{message_name}Params(')
      with self.indent():
        self.write(f'const MessageParams &{params_name}')
      self.inline(') const -> ', indent=True)
      self.generate_as_message_params_type(params_spec)
      self.inline(';', end='\n')

  def generate_incoming_request_methods(self: Self) -> None:
    self.write('// ================= //')
    self.write('// Incoming Requests //')
    self.write('// ================= //')
    self.newline()
    for request_spec in self.schema["requests"]:
      if request_spec["messageDirection"] == "clientToServer":
        self.generate_as_incoming_params("requestParams", request_spec)
        request_method = request_spec["method"]
        request_name = method_to_camel_case(request_method)
        result_name = f'{request_name}Result'
        fn_nym = f'{lower_first(result_name)}ToAny'
        if fn_nym not in self.generated_to_any:
          self.write(f'auto {fn_nym}(')
          with self.indent():
            self.write(f'const {result_name} &result')
          self.write(') -> std::unique_ptr<LSPAny>;')
          self.generated_to_any.add(fn_nym)

  def generate_incoming_notification(self: Self) -> None:
    self.write('// ====================== //')
    self.write('// Incoming Notifications //')
    self.write('// ====================== //')
    self.newline()
    for notification_spec in self.schema["notifications"]:
      if notification_spec["messageDirection"] == "clientToServer":
        self.generate_as_incoming_params("notificationParams", notification_spec)

  def generate_as_outgoing_params(
      self: Self,
      params_name: str,
      message_spec: Dict[str, Any]
  ) -> None:
    message_name = method_to_camel_case(message_spec["method"])
    params_spec = message_spec.get("params", None)
    if params_spec is not None:
      self.write(f'auto asMessageParams(')
      with self.indent():
        self.write(f'const {params_spec["name"]} &{params_name}')
      self.write(') const -> MessageParams;')

  def generate_outgoing_request_methods(self: Self) -> None:
    self.write('// ================= //')
    self.write('// Outgoing Requests //')
    self.write('// ================= //')
    self.newline()
    for request_spec in self.schema["requests"]:
      if request_spec["messageDirection"] == "serverToClient":
        self.generate_as_outgoing_params("requestParams", request_spec)
        result_spec = request_spec.get("result", None)
        if result_spec is not None:
          request_method = request_spec["method"]
          request_name = method_to_camel_case(request_method)
          result_name = f'{request_name}Result'
          symbol_name = result_name
          symbol_spec = result_spec
          symbol_kind = symbol_spec["kind"]
          while (symbol_kind == "reference") and (symbol_name in self.symbols):
            symbol_kind, symbol_spec = self.symbols[symbol_name]
            symbol_name = symbol_spec["name"]
          self.write(f'auto anyTo{upper_first_char(result_name)}(')
          with self.indent():
            self.write(f'const LSPAny &any')
          if symbol_kind == "structure":
            self.write(f') const -> std::unique_ptr<{result_name}>;')
          else:
            self.write(f') const -> {result_name};')

  def generate_outgoing_notification(self: Self) -> None:
    self.write('// ====================== //')
    self.write('// Outgoing Notifications //')
    self.write('// ====================== //')
    self.newline()
    for notification_spec in self.schema["notifications"]:
      if notification_spec["messageDirection"] == "serverToClient":
        self.generate_as_outgoing_params("notificationParams", notification_spec)

  def generate_enumeration_transforms(self: Self) -> None:
    self.write('// ===================================== //')
    self.write('// LSPAny <-> LSP Enumeration Transforms //')
    self.write('// ===================================== //')
    self.newline()
    enum_specs = chain(
      DEFAULT_SCHEMA["enumerations"],
      self.schema["enumerations"],
    )
    for enum_spec in enum_specs:
      enum_name = enum_spec["name"]
      self.write(f'auto anyTo{upper_first_char(enum_name)}(')
      with self.indent():
        self.write('const LSPAny &any')
      self.write(f') const -> {enum_name};')
      fn_nym = f'{lower_first(enum_name)}ToAny'
      self.write(f'auto {fn_nym}(')
      with self.indent():
        self.write(f'{enum_name} enumerator')
      self.write(f') const -> std::unique_ptr<LSPAny>;')
      self.generated_to_any.add(fn_nym)

  def extract_nested_dependencies(
      self: Self,
      nested_dependencies: Deque[
        Tuple[
          Deque[str],
          Callable[
            [
              Deque[str],
              Dict[str, Any]
            ],
            None
          ],
          Dict[str, Any]
        ]
      ],
      spec: Dict[str, Any]
  ) -> None:
    match spec["kind"]:
      case "base" | "stringLiteral" | "integerLiteral" | "booleanLiteral" | "reference":
        pass
      case "array":
        self.extract_nested_dependencies(nested_dependencies, spec["element"])
      case "map":
        self.extract_nested_dependencies(nested_dependencies, spec["key"])
        self.extract_nested_dependencies(nested_dependencies, spec["value"])
      case "and":
        raise ValueError(
          f'AND types are not supported for type declarations: {spec}'
        )
      case "or":
        nested_dependencies.append((
          deque(self.nested_names),
          self.generate_nested_variant,
          spec
        ))
        for index, item_spec in enumerate(spec["items"]):
          with self.nest_name(str(index)):
            self.extract_nested_dependencies(nested_dependencies, item_spec)
      case "tuple":
        for item_spec in spec["items"]:
          self.extract_nested_dependencies(nested_dependencies, item_spec)
      case "literal":
        nested_dependencies.append((
          deque(self.nested_names),
          self.generate_nested_structure,
          spec
        ))
        for prop_spec in spec["value"]["properties"]:
          with self.nest_name(prop_spec["name"]):
            self.extract_nested_dependencies(nested_dependencies, prop_spec["type"])
      case _:
        raise ValueError(f'Unsupported Type kind: {spec}')

  def generate_nested_variant(
      self: Self,
      nested_names: Deque[str],
      spec: Dict[str, Any]
  ) -> None:
    with self.nested_names_as(nested_names) as nested_name:
      self.write(f'auto anyTo{upper_first_char(nested_name)}(')
      with self.indent():
        self.write('const LSPAny &any')
      self.write(f') const -> {nested_name};')
      fn_nym = f'{lower_first(nested_name)}ToAny'
      self.write(f'auto {fn_nym}(')
      with self.indent():
        self.write(f'const {nested_name} &variant')
      self.write(f') const -> std::unique_ptr<LSPAny>;')
      self.generated_to_any.add(fn_nym)

  def generate_nested_structure(
      self: Self,
      nested_names: Deque[str],
      spec: Dict[str, Any]
  ) -> None:
    with self.nested_names_as(nested_names) as nested_name:
      self.write(f'auto anyTo{upper_first_char(nested_name)}(')
      with self.indent():
        self.write('const LSPAny &any')
      self.write(f') const -> std::unique_ptr<{nested_name}>;')
      fn_nym = f'{lower_first(nested_name)}ToAny'
      self.write(f'auto {fn_nym}(')
      with self.indent():
        self.write(f'const {nested_name} &structure')
      self.write(f') const -> std::unique_ptr<LSPAny>;')
      self.generated_to_any.add(fn_nym)

  def generate_structure_transforms(self: Self) -> None:
    self.write('// =================================== //')
    self.write('// LSPAny <-> LSP Structure Transforms //')
    self.write('// =================================== //')
    self.newline()
    struct_specs = chain(
      DEFAULT_SCHEMA["structures"],
      self.schema["structures"],
    )
    for struct_spec in struct_specs:
      with self.nested_names_as(deque([struct_spec["name"]])) as struct_name:
        nested_dependencies = deque()
        pending = deque([struct_spec])
        while len(pending) > 0:
          spec = pending.popleft()
          for prop_spec in reversed(spec["properties"]):
            with self.nest_name(prop_spec["name"]):
              self.extract_nested_dependencies(nested_dependencies, prop_spec["type"])
          spec_mixins = spec.get("mixins", None)
          if spec_mixins is not None:
            for type_spec in spec_mixins:
              mixin_name = type_spec["name"]
              _, mixin_spec = self.symbols[mixin_name]
              pending.append(mixin_spec)
        while len(nested_dependencies) > 0:
          nested_names, generate_fn, nested_spec = nested_dependencies.pop()
          generate_fn(nested_names, nested_spec)
        self.generate_nested_structure(deque([struct_name]), struct_spec)

  def generate_type_alias_transforms(self: Self) -> None:
    self.write('// ==================================== //')
    self.write('// LSPAny <-> LSP Type Alias Transforms //')
    self.write('// ==================================== //')
    self.newline()
    alias_specs = chain(
      DEFAULT_SCHEMA["typeAliases"],
      self.schema["typeAliases"],
    )
    for alias_spec in alias_specs:
      with self.nested_names_as(deque([alias_spec["name"]])) as alias_name:
        match alias_name:
          case "LSPAny" | "LSPObject" | "LSPArray":
            pass
          case _:
            nested_dependencies = deque()
            type_spec = alias_spec["type"]
            self.extract_nested_dependencies(nested_dependencies, type_spec)
            while len(nested_dependencies) > 0:
              nested_names, generate_fn, nested_spec = nested_dependencies.pop()
              generate_fn(nested_names, nested_spec)
            if type_spec["kind"] != "or":
              self.write(f'auto anyTo{upper_first_char(alias_name)}(')
              with self.indent():
                self.write('const LSPAny &any')
              # self.write(f') const -> {rename_type(alias_name)};')
              self.inline(') const -> ', indent=True)
              self.generate_type_declaration(type_spec)
              self.inline(';', end='\n')
              fn_nym = f'{lower_first(alias_name)}ToAny'
              self.write(f'auto {fn_nym}(')
              with self.indent():
                self.write(f'const {rename_type(alias_name)} &alias')
              self.write(') const -> std::unique_ptr<LSPAny>;')
              self.generated_to_any.add(fn_nym)

  def generate_copy_methods(self: Self) -> None:
    self.write('// ============ //')
    self.write('// Copy Methods //')
    self.write('// ============ //')
    self.newline()
    self.write('auto copy(const std::unique_ptr<LSPAny> &any) const -> std::unique_ptr<LSPAny>;')
    self.write('auto copy(const LSPObject &object) const -> LSPObject;')
    self.write('auto copy(const LSPArray &array) const -> LSPArray;')

  def generate_code(self: Self) -> None:
    print(f'Generating: {self.file_path}')
    self.write('// -----------------------------------------------------------------------------')
    self.write('// NOTE: This file was generated from Microsoft\'s Language Server Protocol (LSP)')
    self.write('// specification. Please do not edit it by hand.')
    self.write('// -----------------------------------------------------------------------------')
    self.newline()
    self.write('#pragma once')
    self.newline()
    self.write('#include <cstddef>')
    self.write('#include <memory>')
    self.newline()
    self.write('#include <server/logger.h>')
    self.write('#include <server/specification.h>')
    self.newline()
    self.write(f'namespace {self.namespace} {{')
    self.newline()
    with self.indent():
      self.write('namespace lsl = LCompilers::LLanguageServer::Logging;')
      self.newline()
      self.write('class LspTransformer {')
      self.write('public:')
      with self.indent():
        self.write('LspTransformer(lsl::Logger &logger);')
        self.newline()
        self.generate_copy_methods()
        self.newline()
        self.generate_enumeration_transforms()
        self.newline()
        self.generate_structure_transforms()
        self.newline()
        self.generate_type_alias_transforms()
        self.newline()
        self.generate_incoming_request_methods()
        self.newline()
        self.generate_incoming_notification()
        self.newline()
        self.generate_outgoing_request_methods()
        self.newline()
        self.generate_outgoing_notification()
        self.newline()
      self.write('private:')
      with self.indent():
        self.write('lsl::Logger &logger;')
      self.write('}; // class LspTransformer')
      self.newline()
    self.write(f'}} // namespace {self.namespace}')

class CPlusPlusLspTransformerSourceGenerator(CPlusPlusFileGenerator):
  generated_to_any: Set[str]

  def __init__(
      self: Self,
      output_dir: Path,
      schema: Dict[str, Any],
      namespace: str,
      symbols: Dict[str, Tuple[str, Dict[str, Any]]]
  ) -> None:
    specification_source = output_dir / "lsp_transformer.cpp"
    super().__init__(specification_source, schema, namespace, symbols)
    self.generated_to_any = set()

  def generate_enumeration_transforms(self: Self) -> None:
    enum_specs = chain(
      DEFAULT_SCHEMA["enumerations"],
      self.schema["enumerations"],
    )
    for enum_spec in enum_specs:
      enum_name = enum_spec["name"]
      inst_name = lower_first(enum_name)
      type_spec = enum_spec["type"]
      type_name = type_spec["name"]
      enumerator = rename_enum(type_name)
      value_type = rename_type(type_name)
      self.write(f'auto LspTransformer::anyTo{upper_first_char(enum_name)}(')
      with self.indent():
        self.write('const LSPAny &any')
      self.write(f') const -> {enum_name} {{')
      with self.indent():
        self.write('try {')
        with self.indent():
          self.write('switch (static_cast<LSPAnyType>(any.index())) {')
          match type_name:
            case "string":
              self.write(f'case LSPAnyType::{any_enum("string")}: {{')
              with self.indent():
                self.write(f'const {value_type} &value = std::get<std::string>(any);')
                self.write(f'return {inst_name}ByValue(value);')
                self.write('break;')
              self.write('}')
            case "integer":
              self.write(f'case LSPAnyType::{any_enum("integer")}: {{')
              with self.indent():
                self.write(f'{value_type} value = std::get<{value_type}>(any);')
                self.write(f'return {inst_name}ByValue(value);')
                self.write('break;')
              self.write('}')
              self.write(f'case LSPAnyType::{any_enum("uinteger")}: {{')
              with self.indent():
                self.write(f'{value_type} value = static_cast<{value_type}>(')
                with self.indent(): self.write(f'std::get<{rename_type("uinteger")}>(any)')
                self.write(');')
                self.write(f'return {inst_name}ByValue(value);')
                self.write('break;')
              self.write('}')
            case "uinteger":
              self.write(f'case LSPAnyType::{any_enum("uinteger")}: {{')
              with self.indent():
                self.write(f'{value_type} value = std::get<{value_type}>(any);')
                self.write(f'return {inst_name}ByValue(value);')
                self.write('break;')
              self.write('}')
              self.write(f'case LSPAnyType::{any_enum("integer")}: {{')
              with self.indent():
                self.write(f'{value_type} value = static_cast<{value_type}>(')
                with self.indent(): self.write(f'std::get<{rename_type("integer")}>(any)')
                self.write(');')
                self.write(f'return {inst_name}ByValue(value);')
                self.write('break;')
              self.write('}')
            case _:
              raise ValueError(f'Unsupported enumeration type ({type_name}): {enum_spec}')
          self.write('default: {')
          with self.indent():
            self.write('throw LSP_EXCEPTION(')
            with self.indent():
              self.write('ErrorCodes::INVALID_PARAMS,')
              self.write(f'("LSPAnyType for a(n) {enum_name} must be of type LSPAnyType::{enumerator} but received type " +')
              self.write(f' LSPAnyTypeNames.at(static_cast<LSPAnyType>(any.index())))')
            self.write(');')
          self.write('}')
          self.write('}')
        self.write('} catch (std::invalid_argument &e) {')
        with self.indent():
          self.write('throw LSP_EXCEPTION(')
          with self.indent():
            self.write('ErrorCodes::INVALID_PARAMS,')
            self.write('e.what()')
          self.write(');')
        self.write('}')
      self.write('}')
      self.newline()
      fn_nym = f'{lower_first(enum_name)}ToAny'
      self.write(f'auto LspTransformer::{fn_nym}(')
      with self.indent():
        self.write(f'{enum_name} enumerator')
      self.write(') const -> std::unique_ptr<LSPAny> {')
      with self.indent():
        self.write('std::unique_ptr<LSPAny> any = std::make_unique<LSPAny>();')
        if type_name == "string":
          self.write(f'(*any) = {enum_name}Values.at(enumerator);')
        else:
          self.write(f'(*any) = static_cast<{value_type}>(enumerator);')
        self.write('return any;')
      self.write('}')
      self.generated_to_any.add(fn_nym)
      self.newline()

  def extract_nested_dependencies(
      self: Self,
      nested_dependencies: Deque[
        Tuple[
          Deque[str],
          Callable[
            [
              Deque[str],
              Dict[str, Any]
            ],
            None
          ],
          Dict[str, Any]
        ]
      ],
      spec: Dict[str, Any]
  ) -> None:
    match spec["kind"]:
      case "base" | "stringLiteral" | "integerLiteral" | "booleanLiteral" | "reference":
        pass
      case "array":
        self.extract_nested_dependencies(nested_dependencies, spec["element"])
      case "map":
        self.extract_nested_dependencies(nested_dependencies, spec["key"])
        self.extract_nested_dependencies(nested_dependencies, spec["value"])
      case "and":
        raise ValueError(
          f'AND types are not supported for type declarations: {spec}'
        )
      case "or":
        nested_dependencies.append((
          deque(self.nested_names),
          self.generate_nested_variant,
          spec
        ))
        for index, item_spec in enumerate(spec["items"]):
          with self.nest_name(str(index)):
            self.extract_nested_dependencies(nested_dependencies, item_spec)
      case "tuple":
        for item_spec in spec["items"]:
          self.extract_nested_dependencies(nested_dependencies, item_spec)
      case "literal":
        nested_dependencies.append((
          deque(self.nested_names),
          self.generate_nested_structure,
          spec
        ))
        for prop_spec in spec["value"]["properties"]:
          with self.nest_name(prop_spec["name"]):
            self.extract_nested_dependencies(nested_dependencies, prop_spec["type"])
      case _:
        raise ValueError(f'Unsupported Type kind: {spec}')

  def get_type_name(self: Self, type_spec: Dict[str, Any]) -> str:
    match type_spec["kind"]:
      case "base" | "reference":
        return type_spec["name"]
      case "literal":
        return self.nested_name()
      case _:
        raise ValueError(f'Cannot determine type name for: {type_spec}')

  def generate_upper_type_name(self: Self, type_spec: Dict[str, Any]) -> None:
    type_name = self.get_type_name(type_spec)
    self.inline(upper_first_char(type_name))

  def generate_lower_type_name(self: Self, type_spec: Dict[str, Any]) -> None:
    type_name = self.get_type_name(type_spec)
    self.inline(lower_first(type_name))

  def index_by_type(
      self: Self,
      type_index: Dict[str, List[Any]],
      type_spec: Dict[str, Any],
      level: int = 0
  ) -> None:
    type_kind = type_spec["kind"]
    match type_kind:
      case "base":
        type_name = type_spec["name"]
        match type_name:
          case "string" | "URI" | "DocumentUri" | "RegExp":
            index = type_index["string"]
          case _:
            index = type_index[type_name]
        index.append(("base", type_spec))
      case "reference":
        type_name = type_spec["name"]
        symbol_kind, symbol_spec = self.symbols[type_name]
        match symbol_kind:
          case "enumeration":
            index = type_index[symbol_spec["type"]["name"]]
            index.append((symbol_kind, symbol_spec))
          case "union":
            if level == 0:
              for i, item_spec in enumerate(symbol_spec["items"]):
                with self.nest_name(str(i)):
                  self.index_by_type(type_index, item_spec, 1 + level)
            else:
              index = type_index[symbol_kind]
              index.append((type_name, symbol_spec))
          case "structure":
            index = type_index[symbol_kind]
            index.append((type_name, symbol_spec))
          case "literal":
            index = type_index[symbol_kind]
            index.append((self.nested_name(), symbol_spec))
          case "alias":
            match type_name:
              case "LSPAny" | "LSPObject":
                index = type_index["structure"]
                index.append((type_name, symbol_spec))

              case "LSPArray":
                elem_spec = symbol_spec["type"]["element"]
                elem_index = defaultdict(list)
                self.index_by_type(elem_index, elem_spec, 1 + level)
                index = type_index["array"]
                index.append((symbol_spec, elem_index))
              case _:
                self.index_by_type(type_index, symbol_spec["type"], 1 + level)
          case "request":
            raise ValueError(f'Cannot index requests: {type_spec}')
          case "notification":
            raise ValueError(f'Cannot index notifications: {type_spec}')
      case "array":
        elem_spec = type_spec["element"]
        elem_index = defaultdict(list)
        self.index_by_type(elem_index, elem_spec, 1 + level)
        index = type_index[type_kind]
        index.append((type_spec, elem_index))
      case "map":
        key_spec = type_spec["key"]
        key_index = defaultdict(list)
        self.index_by_type(key_index, key_spec, 1 + level)
        value_spec = type_spec["value"]
        value_index = defaultdict(list)
        self.index_by_type(value_index, value_spec, 1 + level)
        index = type_index[type_kind]
        index.append((type_spec, key_index, value_index))
      case "and":
        raise ValueError(f'AND types are not supported: {type_spec}')
      case "or":
        for i, item_spec in enumerate(type_spec["items"]):
          with self.nest_name(str(i)):
            self.index_by_type(type_index, item_spec, 1 + level)
      case "tuple":
        item_indices = []
        for i, item_spec in enumerate(type_spec["items"]):
          with self.nest_name(str(i)):
            item_index = defaultdict(list)
            self.index_by_type(item_index, item_spec, 1 + level)
            item_indices.append(item_index)
        index = type_index[type_kind]
        index.append((self.nested_name(), type_spec, item_indices))
      case "literal":
        index = type_index[type_kind]
        index.append((self.nested_name(), type_spec))
      case "stringLiteral":
        index = type_index["string"]
        index.append(type_spec)
      case "integerLiteral":
        index = type_index["integer"]
        index.append(type_spec)
      case "booleanLiteral":
        index = type_index["boolean"]
        index.append(type_spec)

  def expand_fields(
      self: Self,
      symbol_kind: str,
      type_spec: Dict[str, Any]
  ) -> Iterator[Tuple[str, Dict[str, Any]]]:
    match symbol_kind:
      case "structure":
        type_name = type_spec["name"]
        pending = deque([(type_name, symbol_kind, type_spec)])
        visited_type_names = set()
        visited_field_names = set()
        while len(pending) > 0:
          spec_name, spec_kind, spec = pending.popleft()
          if spec_name not in visited_type_names:
            visited_type_names.add(spec_name)
            match spec_kind:
              case "structure":
                super_refs = spec.get("extends", None)
                if super_refs is not None:
                  for super_ref in super_refs:
                    super_name = super_ref["name"]
                    super_kind, super_spec = self.symbols[super_name]
                    pending.append((super_name, super_kind, super_spec))
                mixin_refs = spec.get("mixins", None)
                if mixin_refs is not None:
                  for mixin_ref in mixin_refs:
                    mixin_name = mixin_ref["name"]
                    mixin_kind, mixin_spec = self.symbols[mixin_name]
                    pending.append((mixin_name, mixin_kind, mixin_spec))
                for prop_spec in spec["properties"]:
                  prop_name = prop_spec["name"]
                  if prop_name not in visited_field_names:
                    visited_field_names.add(prop_name)
                    yield spec_name, prop_spec
              case "literal":
                for prop_spec in spec["value"]["properties"]:
                  prop_name = prop_spec["name"]
                  if prop_name not in visited_field_names:
                    visited_field_names.add(prop_name)
                    yield spec_name, prop_spec
              case _:
                pass
      case "literal":
        for prop_spec in type_spec["value"]["properties"]:
          prop_name = prop_spec["name"]
          yield self.nested_name(), prop_spec
      case _:
        raise ValueError(f'Unsupported type ({type_spec["kind"]}): {type_spec}')

  def generate_any_to_uinteger(
      self: Self,
      type_name: str,
      uinteger_specs: List[
        Tuple[str, Dict[str, Any]]
      ]
  ) -> None:
    if len(uinteger_specs) > 0:
      spec_type, uinteger_spec = uinteger_specs[0]
      if spec_type == "enumeration":
        self.write('try {')
        with self.indent():
          enum_spec = uinteger_spec
          enum_name = enum_spec["name"]
          self.write(f'value = anyTo{upper_first_char(enum_name)}(any);')
        self.write('} catch (LspException &e) {')
        with self.indent():
          self.generate_any_to_uinteger(type_name, uinteger_specs[1:])
        self.write('}')
      elif len(uinteger_specs) == 1:
        self.write('value = anyToUInteger(any);')
      else:
        raise ValueError(f'Redundant uinteger specs detected')
    else:
      self.write('throw LSP_EXCEPTION(')
      with self.indent():
        self.write('ErrorCodes::INVALID_PARAMS,')
        self.write(f'"Failed to transform LSPAny to {type_name}"')
      self.write(');')

  def generate_any_to_integer(
      self: Self,
      type_name: str,
      integer_specs: List[
        Tuple[str, Dict[str, Any]]
      ]
  ) -> None:
    if len(integer_specs) > 0:
      spec_type, integer_spec = integer_specs[0]
      if spec_type == "enumeration":
        self.write('try {')
        with self.indent():
          enum_spec = integer_spec
          enum_name = enum_spec["name"]
          self.write(f'value = anyTo{upper_first_char(enum_name)}(any);')
        self.write('} catch (LspException &e) {')
        with self.indent():
          self.generate_any_to_integer(type_name, integer_specs[1:])
        self.write('}')
      elif len(integer_specs) == 1:
        self.write('value = anyToInteger(any);')
      else:
        raise ValueError(f'Redundant integer specs detected')
    else:
      self.write('throw LSP_EXCEPTION(')
      with self.indent():
        self.write('ErrorCodes::INVALID_PARAMS,')
        self.write(f'"Failed to transform LSPAny to {type_name}"')
      self.write(');')

  def generate_any_to_string(
      self: Self,
      type_name: str,
      string_specs: List[
        Tuple[str, Dict[str, Any]]
      ]
  ) -> None:
    if len(string_specs) > 0:
      spec_type, string_spec = string_specs[0]
      if spec_type == "enumeration":
        self.write('try {')
        with self.indent():
          enum_spec = string_spec
          enum_name = enum_spec["name"]
          self.write(f'value = anyTo{upper_first_char(enum_name)}(any);')
        self.write('} catch (LspException &e) {')
        with self.indent():
          self.generate_any_to_string(type_name, string_specs[1:])
        self.write('}')
      elif len(string_specs) == 1:
        self.write('value = anyToString(any);')
      else:
        raise ValueError(f'Redundant string specs detected')
    else:
      self.write('throw LSP_EXCEPTION(')
      with self.indent():
        self.write('ErrorCodes::INVALID_PARAMS,')
        self.write(f'"Failed to transform LSPAny to {type_name}"')
      self.write(');')

  def generate_any_to_nested_object(
      self: Self,
      type_name: str,
      struct_specs: Optional[
        List[
          Tuple[str, Dict[str, Any]]
        ]
      ],
      literal_specs: Optional[
        List[
          Tuple[str, Dict[str, Any]]
        ]
      ],
      union_specs: Optional[
        List[
          Tuple[str, Dict[str, Any]]
        ]
      ],
      map_specs: Optional[
        List[
          Tuple[
            Dict[str, Any],
            Dict[str, List[Any]],
            Dict[str, List[Any]]
          ]
        ]
      ]
  ) -> None:
    if struct_specs is not None and len(struct_specs) > 0:
      struct_name, struct_spec = struct_specs[0]
      self.write('try {')
      with self.indent():
        if struct_name == "LSPObject":
          self.write('const LSPObject &object = std::get<LSPObject>(any);')
          self.write(f'value = copy(object);')
        else:
          self.write(f'value = anyTo{struct_name}(any);')
      self.write('} catch (LspException &e) {')
      with self.indent():
        self.generate_any_to_nested_object(
          type_name,
          struct_specs[1:],
          literal_specs,
          union_specs,
          map_specs
        )
      self.write('}')
    elif literal_specs is not None and len(literal_specs) > 0:
      literal_name, literal_spec = literal_specs[0]
      self.write('try {')
      with self.indent():
        self.write(f'value = anyTo{literal_name}(any);')
      self.write('} catch (LspException &e) {')
      with self.indent():
        self.generate_any_to_nested_object(
          type_name,
          struct_specs,
          literal_specs[1:],
          union_specs,
          map_specs
        )
      self.write('}')
    elif union_specs is not None and len(union_specs) > 0:
      union_name, union_spec = union_specs[0]
      self.write('try {')
      with self.indent():
        self.write(f'value = anyTo{union_name}(any);')
      self.write('} catch (LspException &e) {')
      with self.indent():
        self.generate_any_to_nested_object(
          type_name,
          struct_specs,
          literal_specs,
          union_specs[1:],
          map_specs
        )
      self.write('}')
    elif map_specs is not None and len(map_specs) > 0:
      raise ValueError(f'Unsupported spec type (map) for {type_name}: {map_specs}')
    else:
      self.write('throw LSP_EXCEPTION(')
      with self.indent():
        self.write('ErrorCodes::INVALID_PARAMS,')
        self.write(f'"Failed to transform LSPAny to {type_name}"')
      self.write(');')

  def generate_any_to_array(
      self: Self,
      type_name: str,
      array_specs: Optional[
        List[
          Tuple[
            Dict[str, Any],
            Dict[str, List[Any]]
          ]
        ]
      ]
  ) -> None:
    if len(array_specs) > 0:
      array_spec, elem_index = array_specs[0]
      self.write('try {')
      with self.indent():
        match array_spec.get("name", None):
          case "LSPArray":
            self.write('const LSPArray &array = std::get<LSPArray>(any);')
            self.write('value = copy(array);')
          case _:
            elem_spec = array_spec["element"]
            self.inline('std::vector<', indent=True)
            self.generate_type_declaration(elem_spec)
            self.inline('> values;', end='\n')
            self.write('for (const std::unique_ptr<LSPAny> &elem')
            with self.indent(2): self.write(': std::get<LSPArray>(any)) {')
            with self.indent():
              match elem_spec["name"]:
                case "LSPAny":
                  self.write('values.push_back(copy(elem));')
                case "LSPObject":
                  self.write('values.push_back(copy(std::get<LSPObject>(*elem)));')
                case "LSPArray":
                  self.write('values.push_back(copy(std::get<LSPArray>(*elem)));')
                case _:
                  self.inline('values.push_back(anyTo', indent=True)
                  self.generate_upper_type_name(elem_spec)
                  self.inline('(*elem));', end='\n')
            self.write('}')
            self.write(f'value = std::move(values);')
      self.write('} catch (LspException &e) {')
      with self.indent():
        self.generate_any_to_array(type_name, array_specs[1:])
      self.write('}')
    else:
      self.write('throw LSP_EXCEPTION(')
      with self.indent():
        self.write('ErrorCodes::INVALID_PARAMS,')
        self.write(f'"Failed to transform LSPAny to array"')
      self.write(');')

  def generate_any_to_nested_variant(self: Self, spec: Dict[str, Any]) -> None:
    nested_name = self.nested_name()
    type_index = defaultdict(list)
    self.index_by_type(type_index, spec)
    struct_specs = type_index.get("structure", None)
    literal_specs = type_index.get("literal", None)
    union_specs = type_index.get("union", None)
    map_specs = type_index.get("map", None)
    has_object_type = (struct_specs is not None) \
      or (literal_specs is not None) \
      or (union_specs is not None) \
      or (map_specs is not None)

    self.write(f'auto LspTransformer::anyTo{upper_first_char(nested_name)}(')
    with self.indent():
      self.write('const LSPAny &any')
    symbol_name = nested_name
    symbol_kind = spec["kind"]
    while symbol_kind == "reference":
      symbol_kind, symbol_spec = self.symbols[symbol_name]
      symbol_name = symbol_spec["name"]
    if symbol_kind == "structure":
      self.write(f') const -> std::unique_ptr<{nested_name}> {{')
      with self.indent():
        self.write(f'std::unique_ptr<{nested_name}> value;')
    else:
      self.write(f') const -> {nested_name} {{')
      with self.indent():
        self.write(f'{nested_name} value;')
    with self.indent():
      self.newline()
      self.write('switch (static_cast<LSPAnyType>(any.index())) {')
      if has_object_type:
        self.write('case LSPAnyType::OBJECT_TYPE: {')
        with self.indent():
          self.generate_any_to_nested_object(
            nested_name,
            struct_specs,
            literal_specs,
            union_specs,
            map_specs
          )
          self.write('break;')
        self.write('}')
      array_specs = type_index.get("array", None)
      if array_specs is not None:
        self.write('case LSPAnyType::ARRAY_TYPE: {')
        with self.indent():
          self.generate_any_to_array(nested_name, array_specs)
          self.write('break;')
        self.write('}')
      string_specs = type_index.get("string", None)
      if string_specs is not None:
        string_specs.sort(key=lambda pair: pair[0])
        self.write('case LSPAnyType::STRING_TYPE: {')
        with self.indent():
          self.generate_any_to_string(nested_name, string_specs)
          self.write('break;')
        self.write('}')
      integer_specs = type_index.get("integer", None)
      if integer_specs is not None:
        self.write('case LSPAnyType::INTEGER_TYPE: {')
        with self.indent():
          self.generate_any_to_integer(nested_name, integer_specs)
          self.write('break;')
        self.write('}')
      uinteger_specs = type_index.get("uinteger", None)
      if uinteger_specs is not None:
        self.write('case LSPAnyType::UINTEGER_TYPE: {')
        with self.indent():
          self.generate_any_to_uinteger(nested_name, uinteger_specs)
          self.write('break;')
        self.write('}')
      decimal_specs = type_index.get("decimal", None)
      if decimal_specs is not None:
        self.write('case LSPAnyType::DECIMAL_TYPE: {')
        with self.indent():
          self.write('value = anyToDecimal(any);')
          self.write('break;')
        self.write('}')
      boolean_specs = type_index.get("boolean", None)
      if boolean_specs is not None:
        self.write('case LSPAnyType::BOOLEAN_TYPE: {')
        with self.indent():
          self.write('value = anyToBoolean(any);')
          self.write('break;')
        self.write('}')
      null_specs = type_index.get("null", None)
      if null_specs is not None:
        self.write('case LSPAnyType::NULL_TYPE: {')
        with self.indent():
          self.write('value = anyToNull(any);')
          self.write('break;')
        self.write('}')
      self.write('default: {')
      with self.indent():
        self.write('throw LSP_EXCEPTION(')
        with self.indent():
          self.write('ErrorCodes::INVALID_PARAMS,')
          self.write(f'("Invalid LSPAnyType for a(n) {nested_name}: " +')
          self.write(' LSPAnyTypeNames.at(static_cast<LSPAnyType>(any.index())))')
        self.write(');')
      self.write('}')
      self.write('}')
      self.newline()
      self.write('return value;')
    self.write('}')
    self.newline()

  def generate_nested_variant_to_any(self: Self, spec: Dict[str, Any]) -> None:
    nested_name = self.nested_name()
    fn_nym = f'{lower_first(nested_name)}ToAny'
    self.write(f'auto LspTransformer::{fn_nym}(')
    with self.indent():
      self.write(f'const {nested_name} &variant')
    self.write(') const -> std::unique_ptr<LSPAny> {')
    with self.indent():
      self.write(f'switch (static_cast<{nested_name}Type>(variant.index())) {{')
      for i, item_spec in enumerate(spec["items"]):
        with self.nest_name(str(i)):
          self.inline(f'case {nested_name}Type::', indent=True)
          self.generate_variant_enumeration(item_spec)
          self.inline(': {', end='\n')
          with self.indent():
            match item_spec["kind"]:
              case "array":
                elem_spec = item_spec["element"]
                self.write('LSPArray array;')
                self.inline('for (const ', indent=True)
                self.generate_type_declaration(elem_spec)
                self.inline(' &elem', end='\n')
                with self.indent(2):
                  self.inline(': std::get<std::vector<', indent=True)
                  self.generate_type_declaration(elem_spec)
                  self.inline('>>(variant)) {', end='\n')
                with self.indent():
                  self.inline(f'array.push_back(', indent=True)
                  self.generate_lower_type_name(elem_spec)
                  match elem_spec["kind"]:
                    case "base":
                      self.inline('ToAny(elem));', end='\n')
                    case "reference":
                      elem_type_name = elem_spec["name"]
                      symbol_kind, symbol_spec = self.symbols[elem_type_name]
                      if symbol_kind == "structure":
                        self.inline('ToAny(*elem));', end='\n')
                      else:
                        self.inline('ToAny(elem));', end='\n')
                    case _:
                      raise ValueError(f'Unsupported array type ({elem_spec["kind"]}): {elem_spec}')
                self.write('}')
                self.write('std::unique_ptr<LSPAny> any = std::make_unique<LSPAny>();')
                self.write('(*any) = std::move(array);')
                self.write('return any;')
              case "tuple":
                self.write('LSPArray array;')
                item_specs = item_spec["items"]
                if len(item_specs) == 2:
                  self.inline('const std::pair<', indent=True)
                  self.generate_type_declaration(item_specs[0])
                  self.inline(', ')
                  self.generate_type_declaration(item_specs[1])
                  self.inline('> &pair =', end='\n')
                  with self.indent():
                    self.inline('std::get<std::pair<', indent=True)
                    self.generate_type_declaration(item_specs[0])
                    self.inline(', ')
                    self.generate_type_declaration(item_specs[1])
                    self.inline('>>(variant);', end='\n')
                  self.inline(f'array.push_back(', indent=True)
                  self.generate_lower_type_name(item_specs[0])
                  self.inline('ToAny(pair.first));', end='\n')
                  self.inline(f'array.push_back(', indent=True)
                  self.generate_lower_type_name(item_specs[1])
                  self.inline('ToAny(pair.second));', end='\n')
                else:
                  self.inline('const std::tuple<', indent=True)
                  self.generate_type_declaration(item_specs[0])
                  for i in range(1, len(item_specs)):
                    self.inline(', ')
                    self.generate_type_declaration(item_specs[i])
                  self.inline('> &tuple =', end='\n')
                  with self.indent():
                    self.inline('std::get<std::tuple<', indent=True)
                    self.generate_type_declaration(item_specs[0])
                    for i in range(1, len(item_specs)):
                      self.inline(', ')
                      self.generate_type_declaration(item_specs[i])
                    self.inline('>>(variant);', end='\n')
                  for j, item_spec in enumerate(item_specs):
                    self.inline(f'array.push_back(', indent=True)
                    self.generate_lower_type_name(item_spec)
                    self.inline(f'ToAny(std::get<{j}>(tuple)));', end='\n')
                self.write('std::unique_ptr<LSPAny> any = std::make_unique<LSPAny>();')
                self.write('(*any) = std::move(array);')
                self.write('return any;')
              case "reference":
                item_type_name = item_spec["name"]
                match item_type_name:
                  case "LSPAny":
                    self.write(f'return copy(std::get<std::unique_ptr<LSPAny>>(variant));')
                  case "LSPObject" | "LSPArray":
                    self.write('std::unique_ptr<LSPAny> any = std::make_unique<LSPAny>();')
                    self.write(f'(*any) = copy(std::get<{item_type_name}>(variant));')
                    self.write('return any;')
                  case _:
                    symbol_kind, symbol_spec = self.symbols[item_type_name]
                    self.inline(f'return ', indent=True)
                    self.generate_lower_type_name(item_spec)
                    self.inline(f'ToAny(', end='\n')
                    with self.indent():
                      self.inline(indent=True)
                      if symbol_kind == "structure":
                        self.inline('*')
                      self.inline(f'std::get<')
                      self.generate_type_declaration(item_spec)
                      self.inline(f'>(variant)', end='\n')
                    self.write(');')
              case _:
                self.inline(f'return ', indent=True)
                self.generate_lower_type_name(item_spec)
                self.inline(f'ToAny(', end='\n')
                with self.indent():
                  self.inline(indent=True)
                  if item_spec["kind"] == "literal":
                    self.inline('*')
                  self.inline(f'std::get<')
                  self.generate_type_declaration(item_spec)
                  self.inline(f'>(variant)', end='\n')
                self.write(');')
          self.write('}')
      self.write('default: {')
      with self.indent():
        self.write('throw LSP_EXCEPTION(')
        with self.indent():
          self.write('ErrorCodes::INVALID_PARAMS,')
          self.write(f'("Unsupported {nested_name}Type: " +')
          self.write(f' {nested_name}TypeNames.at(static_cast<{nested_name}Type>(variant.index())))')
        self.write(');')
      self.write('}')
      self.write('}')
    self.write('}')
    self.generated_to_any.add(fn_nym)
    self.newline()

  def generate_nested_variant(
      self: Self,
      nested_names: Deque[str],
      spec: Dict[str, Any]
  ) -> None:
    with self.nested_names_as(nested_names) as nested_name:
      self.generate_any_to_nested_variant(spec)
      self.generate_nested_variant_to_any(spec)

  def generate_nested_any_to_structure(
      self: Self,
      spec: Dict[str, Any]
  ) -> None:
    type_name = self.nested_name()
    self.write(f'auto LspTransformer::anyTo{upper_first_char(type_name)}(')
    with self.indent():
      self.write('const LSPAny &any')
    self.write(f') const -> std::unique_ptr<{type_name}> {{')
    with self.indent():
      self.inline(
        'if (static_cast<LSPAnyType>(any.index()) != LSPAnyType::',
        indent=True
      )
      self.inline(rename_enum("object"))
      self.inline(') {', end='\n')
      with self.indent():
        self.write('throw LSP_EXCEPTION(')
        with self.indent():
          self.write('ErrorCodes::INVALID_PARAMS,')
          self.write(f'("LSPAnyType for a(n) {type_name} must be of type LSPAnyType::{rename_enum("object")} but received type " +')
          self.write(' LSPAnyTypeNames.at(static_cast<LSPAnyType>(any.index())))')
        self.write(');')
      self.write('}')
      self.newline()
      self.write(f'std::unique_ptr<{type_name}> value =')
      with self.indent(): self.write(f'std::make_unique<{type_name}>();')
      self.newline()
      self.write('const LSPObject &object = std::get<LSPObject>(any);')
      self.write('LSPObject::const_iterator iter;')
      symbol_kind = spec.get("kind", "structure")
      type_names_and_prop_specs = list(self.expand_fields(symbol_kind, spec))
      self.newline()
      self.write(f'if (object.size() > {len(type_names_and_prop_specs)}) {{')
      with self.indent():
        self.write('throw LSP_EXCEPTION(')
        with self.indent():
          self.write('ErrorCodes::INVALID_PARAMS,')
          self.write(f'"Too many attributes to transform to a(n) {type_name}: " + std::to_string(object.size())')
        self.write(');')
      self.write('}')
      for prop_type_name, prop_spec in type_names_and_prop_specs:
        prop_name = prop_spec["name"]
        prop_type_spec = prop_spec["type"]
        self.newline()
        self.write(f'iter = object.find("{prop_name}");')
        self.write('if (iter != object.end()) {')
        with self.indent():
          match prop_type_spec["kind"]:
            case "base":
              self.inline(f'value->{prop_name} = anyTo', indent=True)
              self.generate_upper_type_name(prop_spec["type"])
              self.inline('(*iter->second);', end='\n')
            case "reference":
              ref_type_name = self.get_type_name(prop_spec["type"])
              match ref_type_name:
                case "LSPAny":
                  self.write(f'value->{prop_name} = copy(iter->second);')
                case "LSPObject":
                  self.write('const LSPObject &object = std::get<LSPObject>(*iter->second);')
                  self.write(f'value->{prop_name} = copy(object);')
                case _:
                  self.inline(f'value->{prop_name} = anyTo', indent=True)
                  self.inline(upper_first_char(ref_type_name))
                  self.inline('(*iter->second);', end='\n')
            case "literal":
              literal_name = self.nested_name([prop_type_name, prop_name])
              self.write(f'value->{prop_name} = anyTo{literal_name}(*iter->second);')
            case "or":
              self.inline(f'value->{prop_name} = anyTo', indent=True)
              self.inline(self.nested_name([prop_type_name, prop_name]))
              self.inline('(*iter->second);', end='\n')
            case "array":
              elem_spec = prop_type_spec["element"]
              self.write('const LSPArray &array = std::get<LSPArray>(*iter->second);')
              match elem_spec["kind"]:
                case "base":
                  self.inline('std::vector<', indent=True)
                  self.generate_type_declaration(elem_spec)
                  self.inline('> values;', end='\n')
                  self.write('for (const std::unique_ptr<LSPAny> &elem : array) {')
                  with self.indent():
                    self.inline('values.push_back(anyTo', indent=True)
                    self.generate_upper_type_name(elem_spec)
                    self.inline('(*elem));', end='\n')
                  self.write('}')
                  self.write(f'value->{prop_name} = std::move(values);')
                case "reference":
                  self.inline('std::vector<', indent=True)
                  self.generate_type_declaration(elem_spec)
                  self.inline('> values;', end='\n')
                  self.write('for (const std::unique_ptr<LSPAny> &elem : array) {')
                  with self.indent():
                    match elem_spec["name"]:
                      case "LSPAny":
                        self.write('values.push_back(copy(elem));')
                      case "LSPObject":
                        self.write('const LSPObject &object = std::get<LSPObject>(*elem);')
                        self.write('values.push_back(copy(object));')
                      case _:
                        self.inline('values.push_back(anyTo', indent=True)
                        self.generate_upper_type_name(elem_spec)
                        self.inline('(*elem));', end='\n')
                  self.write('}')
                  self.write(f'value->{prop_name} = std::move(values);')
                case "or":
                  elem_type_name = self.nested_name([prop_type_name, prop_name])
                  self.write(f'std::vector<{elem_type_name}> values;')
                  self.write('for (const std::unique_ptr<LSPAny> &elem : array) {')
                  with self.indent():
                    if elem_type_name == "LSPAny":
                      self.write(f'values.push_back(copy(elem));')
                    else:
                      self.write(f'values.push_back(anyTo{elem_type_name}(*elem));')
                  self.write('}')
                  self.write(f'value->{prop_name} = std::move(values);')
                case "literal":
                  elem_type_name = self.nested_name([prop_type_name, prop_name])
                  self.write(f'std::vector<std::unique_ptr<{elem_type_name}>> values;')
                  self.write('for (const std::unique_ptr<LSPAny> &elem : array) {')
                  with self.indent():
                    if elem_type_name == "LSPAny":
                      self.write(f'values.push_back(copy(elem));')
                    else:
                      self.write(f'values.push_back(anyTo{elem_type_name}(*elem));')
                  self.write('}')
                  self.write(f'value->{prop_name} = std::move(values);')
                case _:
                  raise ValueError(f'Unsupported array type ({elem_spec["kind"]}): {elem_spec}')
            case "map":
              key_spec = prop_type_spec["key"]
              value_spec = prop_type_spec["value"]
              self.write('const LSPObject &object_map = std::get<LSPObject>(*iter->second);')
              self.inline('std::map<', indent=True)
              self.generate_type_declaration(key_spec)
              self.inline(', ')
              if value_spec["kind"] == "or":
                elem_type_name = self.nested_name([prop_type_name, prop_name])
                self.inline(elem_type_name)
              else:
                self.generate_type_declaration(value_spec)
              self.inline('> map;', end='\n')
              self.write('for (const auto &[map_key, map_value] : object_map) {')
              with self.indent():
                match value_spec["kind"]:
                  case "base" | "reference":
                    self.inline('map.emplace(map_key, anyTo', indent=True)
                    self.generate_upper_type_name(value_spec)
                    self.inline('(*map_value));', end='\n')
                  case "array":
                    elem_spec = value_spec["element"]
                    self.inline('std::vector<', indent=True)
                    self.generate_type_declaration(elem_spec)
                    self.inline('> array;', end='\n')
                    self.write('for (const std::unique_ptr<LSPAny> &elem : std::get<LSPArray>(*map_value)) {')
                    with self.indent():
                      self.inline('array.push_back(anyTo', indent=True)
                      self.generate_upper_type_name(elem_spec)
                      self.inline('(*elem));', end='\n')
                    self.write('}')
                    self.write('map.emplace(map_key, std::move(array));')
                  case "or":
                    elem_type_name = self.nested_name([prop_type_name, prop_name])
                    self.write(f'map.emplace(map_key, anyTo{elem_type_name}(*map_value));')
                  case _:
                    raise ValueError(f'Unsupported map value type ({value_spec["kind"]}): {value_spec}')
              self.write('}')
              self.write(f'value->{prop_name} = std::move(map);')
            case "stringLiteral":
              expected_value = prop_type_spec["value"]
              self.write(f'const {rename_type("string")} &stringValue = anyToString(*iter->second);')
              self.write(f'if (stringValue != "{expected_value}") {{')
              with self.indent():
                self.write('throw LSP_EXCEPTION(')
                with self.indent():
                  self.write('ErrorCodes::INVALID_PARAMS,')
                  self.write(f'"String value for {type_name}.{prop_name} must be \\"{expected_value}\\" but was: \\"" + stringValue + "\\""')
                self.write(');')
              self.write('}')
              self.write(f'value->{prop_name} = stringValue;')
            case _:
              raise ValueError(f'Unsupported property type ({prop_type_spec["kind"]}) for {type_name}.{prop_name}: {prop_spec}')
        if not prop_spec.get("optional", False):
          self.write('} else {')
          with self.indent():
            self.write('throw LSP_EXCEPTION(')
            with self.indent():
              self.write('ErrorCodes::INVALID_PARAMS,')
              self.write(f'"Missing required {type_name} attribute: {prop_name}"')
            self.write(');')
        self.write('}')
      self.newline()
      self.write('return value;')
    self.write('}')
    self.newline()

  def generate_nested_structure_to_any(
      self: Self,
      spec: Dict[str, Any]
  ) -> None:
    type_name = self.nested_name()
    symbol_kind = spec.get("kind", "structure")
    field_types_and_specs = list(self.expand_fields(symbol_kind, spec))
    fn_nym = f'{lower_first(type_name)}ToAny'
    self.write(f'auto LspTransformer::{fn_nym}(')
    with self.indent():
      if len(field_types_and_specs) > 0:
        self.write(f'const {type_name} &structure')
      else:
        self.write(f'const {type_name} &/*structure*/')
    self.write(') const -> std::unique_ptr<LSPAny> {')
    with self.indent():
      self.write('LSPObject object;')
      self.newline()
      for prop_type_name, prop_spec in field_types_and_specs:
        prop_name = prop_spec["name"]
        prop_type = prop_spec["type"]
        is_optional = prop_spec.get("optional", False)
        num_levels = int(is_optional)
        if is_optional:
          self.write(f'if (structure.{prop_name}.has_value()) {{')
        match prop_type["kind"]:
          case "base":
            with self.indent(num_levels):
              self.inline(f'object.emplace("{prop_name}", ', indent=True)
              self.inline(lower_first(prop_type["name"]))
              if is_optional:
                self.inline(f'ToAny(structure.{prop_name}.value()));', end='\n')
              else:
                self.inline(f'ToAny(structure.{prop_name}));', end='\n')
          case "reference":
            with self.indent(num_levels):
              match prop_type["name"]:
                case "LSPAny":
                  self.inline(f'object.emplace("{prop_name}", ', indent=True)
                  if is_optional:
                    self.inline(f'copy(structure.{prop_name}.value()));', end='\n')
                  else:
                    self.inline(f'copy(structure.{prop_name}));', end='\n')
                case "LSPObject" | "LSPArray":
                  if not is_optional:
                    self.write('{')
                  with self.indent(int(not is_optional)):
                    self.write('std::unique_ptr<LSPAny> any = std::make_unique<LSPAny>();')
                    if is_optional:
                      self.write(f'(*any) = copy(structure.{prop_name}.value());')
                    else:
                      self.write(f'(*any) = copy(structure.{prop_name});')
                    self.write(f'object.emplace("{prop_name}", std::move(any));')
                  if not is_optional:
                    self.write('}')
                case _:
                  symbol_kind, symbol_spec = self.symbols[prop_type["name"]]
                  self.inline(f'object.emplace("{prop_name}", ', indent=True)
                  self.inline(lower_first(prop_type["name"]))
                  if symbol_kind == "structure":
                    if is_optional:
                      self.inline(f'ToAny(*structure.{prop_name}.value()));', end='\n')
                    else:
                      self.inline(f'ToAny(*structure.{prop_name}));', end='\n')
                  else:
                    if is_optional:
                      self.inline(f'ToAny(structure.{prop_name}.value()));', end='\n')
                    else:
                      self.inline(f'ToAny(structure.{prop_name}));', end='\n')
          case "or":
            nested_type_name = self.nested_name([prop_type_name, prop_name])
            with self.indent(num_levels):
              self.inline(f'object.emplace("{prop_name}", ', indent=True)
              self.inline(lower_first(nested_type_name))
              if is_optional:
                self.inline(f'ToAny(structure.{prop_name}.value()));', end='\n')
              else:
                self.inline(f'ToAny(structure.{prop_name}));', end='\n')
          case "literal":
            nested_type_name = self.nested_name([prop_type_name, prop_name])
            with self.indent(num_levels):
              self.inline(f'object.emplace("{prop_name}", ', indent=True)
              self.inline(lower_first(nested_type_name))
              if is_optional:
                self.inline(f'ToAny(*structure.{prop_name}.value()));', end='\n')
              else:
                self.inline(f'ToAny(*structure.{prop_name}));', end='\n')
          case "array":
            if not is_optional:
              self.write('{')
              num_levels = 1
            with self.indent(num_levels):
              self.write('LSPArray array;')
              elem_spec = prop_type["element"]
              match elem_spec["kind"]:
                case "base":
                  self.inline('for (const ', indent=True)
                  self.generate_type_declaration(elem_spec)
                  if is_optional:
                    self.inline(f' &elem : structure.{prop_name}.value()) {{', end='\n')
                  else:
                    self.inline(f' &elem : structure.{prop_name}) {{', end='\n')
                  with self.indent():
                    self.inline('array.push_back(', indent=True)
                    self.inline(lower_first(elem_spec["name"]))
                    self.inline(f'ToAny(elem));', end='\n')
                  self.write('}')
                  self.write('std::unique_ptr<LSPAny> any = std::make_unique<LSPAny>();')
                  self.write('(*any) = std::move(array);')
                  self.write(f'object.emplace("{prop_name}", std::move(any));')
                case "reference":
                  self.inline('for (const ', indent=True)
                  self.generate_type_declaration(elem_spec)
                  if is_optional:
                    self.inline(f' &elem : structure.{prop_name}.value()) {{', end='\n')
                  else:
                    self.inline(f' &elem : structure.{prop_name}) {{', end='\n')
                  with self.indent():
                    elem_type_name = elem_spec["name"]
                    match elem_type_name:
                      case "LSPAny" | "LSPObject" | "LSPArray":
                        self.write('array.push_back(copy(elem));')
                      case _:
                        symbol_kind, symbol_spec = self.symbols[elem_type_name]
                        self.inline('array.push_back(', indent=True)
                        self.inline(lower_first(elem_spec["name"]))
                        if symbol_kind == "structure":
                          self.inline(f'ToAny(*elem));', end='\n')
                        else:
                          self.inline(f'ToAny(elem));', end='\n')
                  self.write('}')
                  self.write('std::unique_ptr<LSPAny> any = std::make_unique<LSPAny>();')
                  self.write('(*any) = std::move(array);')
                  self.write(f'object.emplace("{prop_name}", std::move(any));')
                case "or":
                  nested_type_name = self.nested_name([prop_type_name, prop_name])
                  self.inline('for (const ', indent=True)
                  self.inline(nested_type_name)
                  if is_optional:
                    self.inline(f' &elem : structure.{prop_name}.value()) {{', end='\n')
                  else:
                    self.inline(f' &elem : structure.{prop_name}) {{', end='\n')
                  with self.indent():
                    self.inline('array.push_back(', indent=True)
                    self.inline(lower_first(nested_type_name))
                    self.inline(f'ToAny(elem));', end='\n')
                  self.write('}')
                  self.write('std::unique_ptr<LSPAny> any = std::make_unique<LSPAny>();')
                  self.write('(*any) = std::move(array);')
                  self.write(f'object.emplace("{prop_name}", std::move(any));')
                case "literal":
                  nested_type_name = self.nested_name([prop_type_name, prop_name])
                  self.inline('for (const ', indent=True)
                  self.inline(f'std::unique_ptr<{nested_type_name}>')
                  if is_optional:
                    self.inline(f' &elem : structure.{prop_name}.value()) {{', end='\n')
                  else:
                    self.inline(f' &elem : structure.{prop_name}) {{', end='\n')
                  with self.indent():
                    self.inline('array.push_back(', indent=True)
                    self.inline(lower_first(nested_type_name))
                    self.inline(f'ToAny(*elem));', end='\n')
                  self.write('}')
                  self.write('std::unique_ptr<LSPAny> any = std::make_unique<LSPAny>();')
                  self.write('(*any) = std::move(array);')
                  self.write(f'object.emplace("{prop_name}", std::move(any));')
                case _:
                  raise ValueError(f'Unsupported array type ({elem_spec["kind"]}): {elem_spec}')
            if not is_optional:
              self.write('}')
          case "map":
            key_spec = prop_type["key"]
            value_spec = prop_type["value"]
            if not is_optional:
              self.write('{')
            with self.indent():
              self.write('LSPObject map;')
              if is_optional:
                self.write(f'for (const auto &[mapKey, mapValue] : structure.{prop_name}.value()) {{')
              else:
                self.write(f'for (const auto &[mapKey, mapValue] : structure.{prop_name}) {{')
              with self.indent():
                match value_spec["kind"]:
                  case "base":
                    self.inline(f'map.emplace(mapKey, ', indent=True)
                    self.generate_lower_type_name(value_spec)
                    self.inline('ToAny(mapValue));', end='\n')
                  case "reference":
                    self.inline(f'map.emplace(mapKey, ', indent=True)
                    self.generate_lower_type_name(value_spec)
                    self.inline('ToAny(*mapValue));', end='\n')
                  case "array":
                    array_elem_spec = value_spec["element"]
                    self.write('LSPArray array;')
                    self.inline('for (const ', indent=True)
                    self.generate_type_declaration(array_elem_spec)
                    self.inline(' &arrayElem : mapValue) {', end='\n')
                    with self.indent():
                      match array_elem_spec["kind"]:
                        case "base":
                          self.inline(f'array.push_back(', indent=True)
                          self.generate_lower_type_name(array_elem_spec)
                          self.inline('ToAny(arrayElem));', end='\n')
                        case "reference":
                          self.inline(f'array.push_back(', indent=True)
                          self.generate_lower_type_name(array_elem_spec)
                          self.inline('ToAny(*arrayElem));', end='\n')
                        case _:
                          raise ValueError(
                            f'Unsupported array type ({array_elem_spec["kind"]}): {array_elem_spec}'
                          )
                    self.write('}')
                    self.write('std::unique_ptr<LSPAny> arrayAny = std::make_unique<LSPAny>();')
                    self.write('(*arrayAny) = std::move(array);')
                    self.write('map.emplace(mapKey, std::move(arrayAny));')
                  case "or":
                    nested_item_name = self.nested_name([prop_type_name, prop_name])
                    self.inline('map.emplace(mapKey, ', indent=True)
                    self.inline(lower_first(nested_item_name))
                    self.inline('ToAny(mapValue));', end='\n')
                  case _:
                    raise ValueError(f'Unsupported map type ({value_spec["kind"]}): {value_spec}')
              self.write('}')
              self.write('std::unique_ptr<LSPAny> any = std::make_unique<LSPAny>();')
              self.write('(*any) = std::move(map);')
              self.write(f'object.emplace("{prop_name}", std::move(any));')
            if not is_optional:
              self.write('}')
          case "stringLiteral":
            if is_optional:
              self.write(f'object.emplace("{prop_name}", stringToAny(structure.{prop_name}.value()));')
            else:
              self.write(f'object.emplace("{prop_name}", stringToAny(structure.{prop_name}));')
          case _:
            raise ValueError(f'Unsupported type ({prop_type["kind"]}): {prop_spec}')
        if is_optional:
          self.write('}')
      if len(field_types_and_specs) == 0:
        self.write('// empty')
      self.newline()
      self.write('std::unique_ptr<LSPAny> any = std::make_unique<LSPAny>();')
      self.write('(*any) = std::move(object);')
      self.write('return any;')
    self.write('}')
    self.generated_to_any.add(fn_nym)
    self.newline()

  def generate_nested_structure(
      self: Self,
      nested_names: Deque[str],
      spec: Dict[str, Any]
  ) -> None:
    with self.nested_names_as(nested_names) as nested_name:
      self.generate_nested_any_to_structure(spec)
      self.generate_nested_structure_to_any(spec)

  def generate_structure_transforms(self: Self) -> None:
    self.write('// =================================== //')
    self.write('// LSPAny <-> LSP Structure Transforms //')
    self.write('// =================================== //')
    self.newline()
    struct_specs = chain(
      DEFAULT_SCHEMA["structures"],
      self.schema["structures"],
    )
    for struct_spec in struct_specs:
      with self.nested_names_as(deque([struct_spec["name"]])) as struct_name:
        nested_dependencies = deque()
        pending = deque([struct_spec])
        while len(pending) > 0:
          spec = pending.popleft()
          for prop_spec in reversed(spec["properties"]):
            with self.nest_name(prop_spec["name"]):
              self.extract_nested_dependencies(nested_dependencies, prop_spec["type"])
          spec_mixins = spec.get("mixins", None)
          if spec_mixins is not None:
            for type_spec in spec_mixins:
              mixin_name = type_spec["name"]
              _, mixin_spec = self.symbols[mixin_name]
              pending.append(mixin_spec)
        while len(nested_dependencies) > 0:
          nested_names, generate_fn, nested_spec = nested_dependencies.pop()
          generate_fn(nested_names, nested_spec)
        self.generate_nested_structure(deque([struct_name]), struct_spec)

  def generate_any_to_type_alias(
      self: Self,
      alias_name: str,
      alias_spec: Dict[str, Any]
  ) -> None:
    self.write(f'auto LspTransformer::anyTo{upper_first_char(alias_name)}(')
    with self.indent():
      self.write('const LSPAny &any')
    # self.write(f') const -> {rename_type(alias_name)} {{')
    self.inline(') const -> ', indent=True)
    self.generate_type_declaration(alias_spec["type"])
    self.inline(' {', end='\n')
    with self.indent():
      type_spec = alias_spec["type"]
      match type_spec["kind"]:
        case "base":
          type_name = type_spec["name"]
          type_enum = any_enum(alias_name)
          self.write('switch (static_cast<LSPAnyType>(any.index())) {')
          self.write(f'case LSPAnyType::{type_enum}: {{')
          with self.indent():
            self.inline('return std::get<', indent=True)
            self.inline(rename_type(alias_name))
            self.inline('>(any);', end='\n')
          self.write('}')
          match alias_name:
            case "integer":
              integer_type = rename_type("integer")
              self.write(f'case LSPAnyType::{rename_enum("uinteger")}: {{')
              with self.indent():
                uinteger_type = rename_type("uinteger")
                self.write(f'{uinteger_type} value = std::get<{uinteger_type}>(any);')
                self.write(f'return static_cast<{integer_type}>(value);')
              self.write('}')
            case "uinteger":
              uinteger_type = rename_type("uinteger")
              self.write(f'case LSPAnyType::{rename_enum("integer")}: {{')
              with self.indent():
                integer_type = rename_type("integer")
                self.write(f'{integer_type} value = std::get<{integer_type}>(any);')
                self.write(f'return static_cast<{uinteger_type}>(value);')
              self.write('}')
            case "decimal":
              decimal_type = rename_type("decimal")
              self.write(f'case LSPAnyType::{rename_enum("integer")}: {{')
              with self.indent():
                integer_type = rename_type("integer")
                self.write(f'{integer_type} value = std::get<{integer_type}>(any);')
                self.write(f'return static_cast<{decimal_type}>(value);')
              self.write('}')
              self.write(f'case LSPAnyType::{rename_enum("uinteger")}: {{')
              with self.indent():
                uinteger_type = rename_type("uinteger")
                self.write(f'{uinteger_type} value = std::get<{uinteger_type}>(any);')
                self.write(f'return static_cast<{decimal_type}>(value);')
              self.write('}')
          self.write('default: {')
          with self.indent():
            self.write('throw LSP_EXCEPTION(')
            with self.indent():
              self.write('ErrorCodes::INVALID_PARAMS,')
              self.write('("Cannot transform LSPAny of type LSPAnyType::" +')
              self.write(' LSPAnyTypeNames.at(static_cast<LSPAnyType>(any.index())) +')
              self.write(f' " to type {rename_type(alias_name)}")')
            self.write(');')
          self.write('}')
          self.write('}')
        case "reference":
          self.inline('return anyTo', indent=True)
          self.generate_upper_type_name(type_spec)
          self.inline('(any);', end='\n')
        case "array":
          elem_spec = type_spec["element"]
          self.write('const LSPArray &array = std::get<LSPArray>(any);')
          if (elem_spec["kind"] == "reference") \
             and (elem_spec["name"] == "LSPAny"):
            self.write('return copy(array);')
          else:
            self.inline('std::vector<', indent=True)
            self.generate_upper_type_name(elem_spec)
            self.inline('> values;', end='\n')
            self.write('for (const std::unique_ptr<LSPAny> &elem : array) {')
            with self.indent():
              match elem_spec["kind"]:
                case "base" | "reference":
                  self.inline('values.push_back(anyTo', indent=True)
                  self.generate_upper_type_name(elem_spec)
                  self.inline('(*elem));', end='\n')
                case _:
                  raise ValueError(f'Unsupported array type ({elem_spec["kind"]}): {elem_spec}')
            self.write('}')
            self.write('return values;')
        case _:
          raise ValueError(f'Unsupported alias type ({type_spec["kind"]}): {type_spec}')
    self.write('}')
    self.newline()

  def generate_type_alias_to_any(
      self: Self,
      alias_name: str,
      alias_spec: Dict[str, Any]
  ) -> None:
    fn_nym = f'{lower_first(alias_name)}ToAny'
    self.write(f'auto LspTransformer::{fn_nym}(')
    with self.indent():
      self.write(f'const {rename_type(alias_name)} &alias')
    self.write(') const -> std::unique_ptr<LSPAny> {')
    with self.indent():
      type_spec = alias_spec["type"]
      match type_spec["kind"]:
        case "base":
          self.write('std::unique_ptr<LSPAny> any = std::make_unique<LSPAny>();')
          self.write('(*any) = alias;')
          self.write('return any;')
        case "reference":
          self.inline('return ', indent=True)
          self.generate_lower_type_name(type_spec)
          self.inline('ToAny(alias);', end='\n')
        case "array":
          elem_spec = type_spec["element"]
          self.write('LSPArray array;')
          self.inline('for (const ', indent=True)
          self.generate_type_declaration(elem_spec)
          self.inline(' &elem : alias) {', end='\n')
          with self.indent():
            match elem_spec["kind"]:
              case "base":
                self.inline('array.push_back(', indent=True)
                self.generate_lower_type_name(elem_spec)
                self.inline('ToAny(elem));', end='\n')
              case "reference":
                ref_type_name = elem_spec["name"]
                symbol_kind, symbol_spec = self.symbols[ref_type_name]
                self.inline('array.push_back(', indent=True)
                self.generate_lower_type_name(elem_spec)
                if symbol_kind == "structure":
                  self.inline('ToAny(*elem));', end='\n')
                else:
                  self.inline('ToAny(elem));', end='\n')
              case _:
                raise ValueError(f'Unsupported array type ({elem_spec["kind"]}): {elem_spec}')
          self.write('}')
          self.write('std::unique_ptr<LSPAny> any = std::make_unique<LSPAny>();')
          self.write('(*any) = std::move(array);')
          self.write('return any;')
        case _:
          raise ValueError(f'Unsupported alias type ({type_spec["kind"]}): {type_spec}')
    self.write('}')
    self.generated_to_any.add(fn_nym)
    self.newline()

  def generate_type_alias_transforms(self: Self) -> None:
    self.write('// ==================================== //')
    self.write('// LSPAny <-> LSP Type Alias Transforms //')
    self.write('// ==================================== //')
    self.newline()
    alias_specs = chain(
      DEFAULT_SCHEMA["typeAliases"],
      self.schema["typeAliases"],
    )
    for alias_spec in alias_specs:
      with self.nested_names_as(deque([alias_spec["name"]])) as alias_name:
        match alias_name:
          case "LSPAny" | "LSPObject" | "LSPArray":
            pass
          case _:
            nested_dependencies = deque()
            type_spec = alias_spec["type"]
            self.extract_nested_dependencies(nested_dependencies, type_spec)
            while len(nested_dependencies) > 0:
              nested_names, generate_fn, nested_spec = nested_dependencies.pop()
              generate_fn(nested_names, nested_spec)
            if type_spec["kind"] != "or":
              with self.nested_names_as(deque([alias_name])):
                self.generate_any_to_type_alias(alias_name, alias_spec)
                self.generate_type_alias_to_any(alias_name, alias_spec)

  def generate_copy_any(self: Self) -> None:
    self.write('auto LspTransformer::copy(const std::unique_ptr<LSPAny> &any) const -> std::unique_ptr<LSPAny> {')
    with self.indent():
      self.write('std::unique_ptr<LSPAny> clone = std::make_unique<LSPAny>();')
      self.write('switch (static_cast<LSPAnyType>(any->index())) {')
      self.write('case LSPAnyType::OBJECT_TYPE: {')
      with self.indent():
        self.write('(*clone) = copy(std::get<LSPObject>(*any));')
        self.write('break;')
      self.write('}')
      self.write('case LSPAnyType::ARRAY_TYPE: {')
      with self.indent():
        self.write('(*clone) = copy(std::get<LSPArray>(*any));')
        self.write('break;')
      self.write('}')
      self.write('case LSPAnyType::STRING_TYPE: {')
      with self.indent():
        self.write(f'(*clone) = std::get<{rename_type("string")}>(*any);')
        self.write('break;')
      self.write('}')
      self.write('case LSPAnyType::INTEGER_TYPE: {')
      with self.indent():
        self.write(f'(*clone) = std::get<{rename_type("integer")}>(*any);')
        self.write('break;')
      self.write('}')
      self.write('case LSPAnyType::UINTEGER_TYPE: {')
      with self.indent():
        self.write(f'(*clone) = std::get<{rename_type("uinteger")}>(*any);')
        self.write('break;')
      self.write('}')
      self.write('case LSPAnyType::DECIMAL_TYPE: {')
      with self.indent():
        self.write(f'(*clone) = std::get<{rename_type("decimal")}>(*any);')
        self.write('break;')
      self.write('}')
      self.write('case LSPAnyType::BOOLEAN_TYPE: {')
      with self.indent():
        self.write(f'(*clone) = std::get<{rename_type("boolean")}>(*any);')
        self.write('break;')
      self.write('}')
      self.write('case LSPAnyType::NULL_TYPE: {')
      with self.indent():
        self.write(f'(*clone) = std::get<{rename_type("null")}>(*any);')
        self.write('break;')
      self.write('}')
      self.write('}')
      self.write('return clone;')
    self.write('}')

  def generate_copy_object(self: Self) -> None:
    self.write('auto LspTransformer::copy(const LSPObject &object) const -> LSPObject {')
    with self.indent():
      self.write('LSPObject clone;')
      self.write('for (const auto &[key, value] : object) {')
      with self.indent():
        self.write('clone.emplace(key, copy(value));')
      self.write('}')
      self.write('return clone;')
    self.write('}')

  def generate_copy_array(self: Self) -> None:
    self.write('auto LspTransformer::copy(const LSPArray &array) const -> LSPArray {')
    with self.indent():
      self.write('LSPArray clone;')
      self.write('for (const std::unique_ptr<LSPAny> &elem : array) {')
      with self.indent():
        self.write('clone.push_back(copy(elem));')
      self.write('}')
      self.write('return clone;')
    self.write('}')

  def generate_copy_methods(self: Self) -> None:
    self.write('// ============ //')
    self.write('// Copy Methods //')
    self.write('// ============ //')
    self.newline()
    self.generate_copy_any()
    self.newline()
    self.generate_copy_object()
    self.newline()
    self.generate_copy_array()
    self.newline()

  def generate_as_message_params_type(self: Self, type_spec: Dict[str, Any]) -> None:
    match type_spec["kind"]:
      case "reference":
        self.inline(f'std::unique_ptr<{type_spec["name"]}>')
      case _:
        raise ValueError(f'Unsupported request parameter type: {type_spec}')

  def generate_as_incoming_params(
      self: Self,
      message_params_name: str,
      message_spec: Dict[str, Any]
  ) -> None:
    message_name = method_to_camel_case(message_spec["method"])
    params_spec = message_spec.get("params", None)
    if params_spec is not None:
      incoming_params_name = lower_first(self.get_type_name(params_spec))
      self.write(f'auto LspTransformer::as{message_name}Params(')
      with self.indent():
        self.write(f'const MessageParams &{message_params_name}')
      self.inline(') const -> ', indent=True)
      self.generate_as_message_params_type(params_spec)
      self.inline(' {', end='\n')
      with self.indent():
        self.inline(f'if (static_cast<MessageParamsType>({message_params_name}.index()) != MessageParamsType::', indent=True)
        match params_spec["kind"]:
          case "reference":
            self.inline(rename_enum("object"))
          case _:
            raise ValueError(f'Unsupported message parameter type ({params_spec["name"]}): {params_spec}')
        self.inline(') {', end='\n')
        with self.indent():
          self.write('throw LSP_EXCEPTION(')
          with self.indent():
            self.write('ErrorCodes::INVALID_PARAMS,')
            self.write('("Message parameter type must be MessageParamsType::" +')
            self.inline(' MessageParamsTypeNames.at(MessageParamsType::', indent=True)
            match params_spec["kind"]:
              case "reference":
                self.inline(rename_enum("object"))
              case _:
                raise ValueError(f'Unsupported message parameter type ({params_spec["name"]}): {params_spec}')
            self.inline(') +', end='\n')
            self.write(f' " for method=\\"{message_spec["method"]}\\" but received type " +')
            self.write(f' "MessageParamsType::" + MessageParamsTypeNames.at(static_cast<MessageParamsType>({message_params_name}.index())))')
          self.write(');')
        self.write('}')
        self.newline()
        match params_spec["kind"]:
          case "reference":
            symbol_name = params_spec["name"]
            symbol_kind, symbol_spec = self.symbols[symbol_name]
            field_types_and_specs = list(self.expand_fields(symbol_kind, symbol_spec))
            if len(field_types_and_specs) > 0:
              self.write(f'const LSPObject &object = std::get<LSPObject>({message_params_name});')
              self.write('LSPObject::const_iterator iter;')
              self.newline()
            self.inline(indent=True)
            self.generate_as_message_params_type(params_spec)
            self.inline(f' {incoming_params_name} =', end='\n')
            with self.indent():
              self.inline('std::make_unique<', indent=True)
              match params_spec["kind"]:
                case "base" | "reference":
                  self.inline(params_spec["name"])
                case _:
                  raise ValueError(f'Unsupported parameter type ({params_spec["kind"]}): {params_spec}')
              self.inline('>();', end='\n')
            self.newline()
            for prop_type_name, prop_spec in field_types_and_specs:
              prop_name = prop_spec["name"]
              prop_type = prop_spec["type"]
              self.write(f'iter = object.find("{prop_name}");')
              self.write('if (iter != object.end()) {')
              with self.indent():
                match prop_type["kind"]:
                  case "base":
                    self.inline(f'{incoming_params_name}->{prop_name} = anyTo', indent=True)
                    self.generate_upper_type_name(prop_type)
                    self.inline('(*iter->second);', end='\n')
                  case "reference":
                    match prop_type["name"]:
                      case "LSPAny":
                        self.write(f'{incoming_params_name}->{prop_name} = copy(iter->second);')
                      case "LSPObject" | "LSPArray":
                        self.write(f'{incoming_params_name}->{prop_name} = copy(*iter->second);')
                      case _:
                        self.inline(f'{incoming_params_name}->{prop_name} = anyTo', indent=True)
                        self.generate_upper_type_name(prop_spec["type"])
                        self.inline('(*iter->second);', end='\n')
                  case "array":
                    elem_spec = prop_type["element"]
                    self.write('const LSPArray &array = std::get<LSPArray>(*iter->second);')
                    self.inline('std::vector<', indent=True)
                    match elem_spec["kind"]:
                      case "base" | "reference":
                        self.generate_type_declaration(elem_spec)
                      case _:
                        raise ValueError(f'Unsupported array type ({elem_spec["kind"]}): {elem_spec}')
                    self.inline('> values;', end='\n')
                    self.write('for (const std::unique_ptr<LSPAny> &elem : array) {')
                    with self.indent():
                      match elem_spec["name"]:
                        case "LSPAny":
                          self.write('values.push_back(copy(elem));')
                        case "LSPObject":
                          self.write('values.push_back(copy(std::get<LSPObject>(*elem)));')
                        case "LSPArray":
                          self.write('values.push_back(copy(std::get<LSPArray>(*elem)));')
                        case _:
                          self.inline('values.push_back(anyTo', indent=True)
                          self.generate_upper_type_name(elem_spec)
                          self.inline('(*elem));', end='\n')
                    self.write('}')
                    self.write(f'{incoming_params_name}->{prop_name} = std::move(values);')
                  case "or" | "literal":
                    or_type_name = self.nested_name([prop_type_name, prop_name])
                    self.inline(f'{incoming_params_name}->{prop_name} = anyTo', indent=True)
                    self.inline(upper_first_char(or_type_name))
                    self.inline('(*iter->second);', end='\n')
                  case _:
                    raise ValueError(f'Unsupported property kind ({prop_type["kind"]}): {prop_type}')
              if not prop_spec.get("optional", False):
                self.write('} else {')
                with self.indent():
                  self.write('throw LSP_EXCEPTION(')
                  with self.indent():
                    self.write('ErrorCodes::INVALID_PARAMS,')
                    self.write(f'"Missing required {params_spec["name"]} attribute: {prop_name}"')
                  self.write(');')
              self.write('}')
              self.newline()
            self.write(f'return {incoming_params_name};')
          case _:
            raise ValueError(f'Unsupported message parameter type ({params_spec["name"]}): {params_spec}')
      self.write('}')
      self.newline()

  def generate_result_to_any(self: Self, request_spec: Dict[str, Any]) -> None:
    request_method = request_spec["method"]
    request_name = method_to_camel_case(request_method)
    result_name = f'{request_name}Result'
    with self.nested_names_as(deque([result_name])):
      result_type = f'{result_name}Type'
      symbol_kind, symbol_spec = self.symbols[result_name]
      fn_nym = f'{lower_first(result_name)}ToAny'
      if fn_nym not in self.generated_to_any:
        self.write(f'auto LspTransformer::{fn_nym}(')
        with self.indent():
          self.write(f'const {result_name} &result')
        self.write(') -> std::unique_ptr<LSPAny> {')
        with self.indent():
          if symbol_kind == "union":
            self.write(f'switch (static_cast<{result_type}>(result.index())) {{')
            for index, item_spec in enumerate(symbol_spec["items"]):
              self.inline(f'case {result_type}::', indent=True)
              self.generate_variant_enumeration(item_spec)
              self.inline(': {', end='\n')
              with self.indent():
                match item_spec["kind"]:
                  case "base":
                    item_name = item_spec["name"]
                    self.inline('return ', indent=True)
                    self.generate_lower_type_name(item_spec)
                    self.inline('ToAny(std::get<')
                    self.generate_type_declaration(item_spec)
                    self.inline('>(result));', end='\n')
                  case "reference":
                    item_name = item_spec["name"]
                    match item_name:
                      case "LSPAny":
                        self.write('return copy(std::get<std::unique_ptr<LSPAny>>(result));')
                      case "LSPObject" | "LSPArray":
                        self.write(f'return copy(std::get<{item_name}>(result));')
                      case _:
                        item_symbol_kind, item_symbol_spec = self.symbols[item_name]
                        self.inline('return ', indent=True)
                        self.generate_lower_type_name(item_spec)
                        self.inline('ToAny(')
                        if item_symbol_kind == "structure":
                          self.inline('*')
                        self.inline('std::get<')
                        self.generate_type_declaration(item_spec)
                        self.inline('>(result));', end='\n')
                  case "array":
                    elem_spec = item_spec["element"]
                    self.write('LSPArray array;')
                    match elem_spec["kind"]:
                      case "base" | "reference":
                        elem_type_name = elem_spec["name"]
                        self.inline(f'for (const ', indent=True)
                        self.generate_type_declaration(elem_spec)
                        self.inline(' &elem', end='\n')
                        with self.indent(2):
                          self.inline(': std::get<', indent=True)
                          self.generate_type_declaration(item_spec)
                          self.inline('>(result)) {', end='\n')
                        with self.indent():
                          self.inline('array.push_back(', indent=True)
                          self.generate_lower_type_name(elem_spec)
                          if elem_spec["kind"] == "reference":
                            elem_symbol_name = elem_type_name
                            elem_symbol_kind, elem_symbol_spec = self.symbols[elem_symbol_name]
                            while elem_symbol_kind == "alias":
                              elem_symbol_name = elem_symbol_spec["type"]["name"]
                              elem_symbol_kind, elem_symbol_spec = self.symbols[elem_symbol_name]
                            if elem_symbol_kind == "structure":
                              self.inline('ToAny(*elem));', end='\n')
                            else:
                              self.inline('ToAny(elem));', end='\n')
                          else:
                            self.inline('ToAny(elem));', end='\n')
                        self.write('}')
                      case "or":
                        union_type_name = self.nested_name([result_name, str(index)])
                        self.inline(f'for (const ', indent=True)
                        self.inline(union_type_name)
                        # self.generate_type_declaration(elem_spec)
                        self.inline(' &elem', end='\n')
                        with self.indent(2):
                          self.inline(': std::get<std::vector<', indent=True)
                          self.inline(union_type_name)
                          self.inline('>>(result)) {', end='\n')
                        with self.indent():
                          self.write(f'switch (static_cast<{union_type_name}Type>(elem.index())) {{')
                          for elem_item_index, elem_item_spec in enumerate(elem_spec["items"]):
                            self.inline(f'case {union_type_name}Type::', indent=True)
                            self.generate_variant_enumeration(elem_item_spec)
                            self.inline(': {', end='\n')
                            with self.indent():
                              match elem_item_spec["kind"]:
                                case "base" | "reference":
                                  elem_item_symbol_name = elem_item_spec["name"]
                                  match elem_item_symbol_name:
                                    case "LSPAny":
                                      self.write(
                                        f'array.push_back(copy(std::get<std::unique_ptr<LSPAny>>(elem)));'
                                      )
                                    case "LSPObject" | "LSPArray":
                                      self.write(
                                        f'array.push_back(copy(std::get<{elem_item_symbol_name}>(elem)));'
                                      )
                                    case _:
                                      elem_item_symbol_kind, elem_item_symbol_spec = \
                                        self.symbols[elem_item_symbol_name]
                                      while elem_item_symbol_kind == "alias":
                                        elem_item_symbol_name = elem_item_symbol_spec["type"]["name"]
                                        elem_item_symbol_kind, elem_item_symbol_spec = \
                                          self.symbols[elem_item_symbol_name]
                                      self.inline('array.push_back(', indent=True)
                                      self.generate_lower_type_name(elem_item_spec)
                                      self.inline('ToAny(')
                                      if elem_item_symbol_kind == "structure":
                                        self.inline('*')
                                      self.inline('std::get<')
                                      self.generate_type_declaration(elem_item_spec)
                                      self.inline('>(elem)));', end='\n')
                                case _:
                                  raise ValueError(
                                    f'Unsupported item kind ({elem_item_spec["kind"]}): {elem_item_spec}'
                                  )
                              self.write('break;')
                            self.write('}')
                          self.write('}')
                        self.write('}')
                      case _:
                        raise ValueError(f'Unsupported array type ({elem_spec["kind"]}): {elem_spec}')
                    self.write('std::unique_ptr<LSPAny> any = std::make_unique<LSPAny>();')
                    self.write('(*any) = std::move(array);')
                    self.write('return any;')
                  case _:
                    raise ValueError(f'Unsupported union type ({item_spec["kind"]}): {item_spec}')
              self.write('}')
            self.write('}')
            self.write('throw LSP_EXCEPTION(')
            with self.indent():
              self.write('ErrorCodes::INTERNAL_ERROR,')
              self.write('"Should be unreachable."')
            self.write(');')
          else:
            match symbol_spec["kind"]:
              case "base" | "reference":
                self.inline('return ', indent=True)
                self.generate_lower_type_name(symbol_spec)
                self.inline('ToAny(result);', end='\n')
              case "array":
                elem_spec = symbol_spec["element"]
                self.write('LSPArray array;')
                match elem_spec["kind"]:
                  case "base":
                    self.inline('for (const ', indent=True)
                    self.generate_type_declaration(elem_spec)
                    self.inline(' &elem : result) {', end='\n')
                    with self.indent():
                      self.inline(f'array.push_back(', indent=True)
                      self.generate_lower_type_name(elem_spec)
                      self.inline('ToAny(elem));')
                    self.write('}')
                  case "reference":
                    self.inline('for (const ', indent=True)
                    self.generate_type_declaration(elem_spec)
                    self.inline(' &elem : result) {', end='\n')
                    with self.indent():
                      self.inline(f'array.push_back(', indent=True)
                      self.generate_lower_type_name(elem_spec)
                      self.inline('ToAny(*elem));')
                    self.write('}')
                  case _:
                    raise ValueError(f'Unsupported array type ({elem_spec["kind"]}): {elem_spec}')
                self.write('std::unique_ptr<LSPAny> any = std::make_unique<LSPAny>();')
                self.write('(*any) = std::move(array);')
                self.write('return any;')
              case _:
                raise ValueError(f'Unsupported symbol kind ({symbol_spec["kind"]}): {symbol_spec}')
        self.write('}')
        self.generated_to_any.add(fn_nym)
        self.newline()

  def generate_incoming_request_methods(self: Self) -> None:
    self.write('// ================= //')
    self.write('// Incoming Requests //')
    self.write('// ================= //')
    self.newline()
    for request_spec in self.schema["requests"]:
      if request_spec["messageDirection"] == "clientToServer":
        self.generate_as_incoming_params("requestParams", request_spec)
        self.generate_result_to_any(request_spec)

  def generate_incoming_notification(self: Self) -> None:
    self.write('// ====================== //')
    self.write('// Incoming Notifications //')
    self.write('// ====================== //')
    self.newline()
    for notification_spec in self.schema["notifications"]:
      if notification_spec["messageDirection"] == "clientToServer":
        self.generate_as_incoming_params("notificationParams", notification_spec)

  def generate_as_outgoing_params(
      self: Self,
      params_name: str,
      message_spec: Dict[str, Any]
  ) -> None:
    message_name = method_to_camel_case(message_spec["method"])
    params_spec = message_spec.get("params", None)
    if params_spec is not None:
      self.write(f'auto LspTransformer::asMessageParams(')
      with self.indent():
        self.write(f'const {params_spec["name"]} &{params_name}')
      self.write(') const -> MessageParams {')
      with self.indent():
        self.write('MessageParams messageParams;')
        match params_spec["kind"]:
          case "reference":
            symbol_name = params_spec["name"]
            symbol_kind, symbol_spec = self.symbols[symbol_name]
            match symbol_name:
              case "LSPAny":
                self.write(f'switch (static_cast<LSPAnyType>({params_name}.index())) {{')
                self.write('case LSPAnyType::OBJECT_TYPE: {')
                with self.indent():
                  self.write(f'const LSPObject &object = std::get<LSPObject>({params_name});')
                  self.write('messageParams = copy(object);')
                  self.write('break;')
                self.write('}')
                self.write('case LSPAnyType::ARRAY_TYPE: {')
                with self.indent():
                  self.write(f'const LSPArray &array = std::get<LSPArray>({params_name});')
                  self.write('messageParams = copy(array);')
                  self.write('break;')
                self.write('}')
                self.write('default: {')
                with self.indent():
                  self.write('throw LSP_EXCEPTION(')
                  with self.indent():
                    self.write('ErrorCodes::INVALID_PARAMS,')
                    self.write(f'("Invalid LSPAny type for {params_spec["name"]}: " +')
                    self.write(f' LSPAnyTypeNames.at(static_cast<LSPAnyType>({params_name}.index())))')
                  self.write(');')
                self.write('}')
                self.write('}')
                self.newline()
              case _:
                self.write('LSPObject object;')
                self.newline()
                for prop_type_name, prop_spec in self.expand_fields(symbol_kind, symbol_spec):
                  prop_name = prop_spec["name"]
                  prop_type = prop_spec["type"]
                  is_optional = prop_spec.get("optional", False)
                  num_levels = int(is_optional)
                  if is_optional:
                    self.write(f'if ({params_name}.{prop_name}.has_value()) {{')
                  with self.indent(num_levels):
                    match prop_type["kind"]:
                      case "base":
                        self.inline(f'object.emplace("{prop_name}", ', indent=True)
                        self.generate_lower_type_name(prop_type)
                        if is_optional:
                          self.inline(f'ToAny({params_name}.{prop_name}.value()));', end='\n')
                        else:
                          self.inline(f'ToAny({params_name}.{prop_name}));', end='\n')
                      case "reference":
                        prop_type_name = prop_type["name"]
                        symbol_kind, symbol_spec = self.symbols[prop_type_name]
                        self.inline(f'object.emplace("{prop_name}", ', indent=True)
                        self.generate_lower_type_name(prop_type)
                        if is_optional:
                          if symbol_kind == "structure":
                            self.inline(f'ToAny(*{params_name}.{prop_name}.value()));', end='\n')
                          else:
                            self.inline(f'ToAny({params_name}.{prop_name}.value()));', end='\n')
                        else:
                          if symbol_kind == "structure":
                            self.inline(f'ToAny(*{params_name}.{prop_name}));', end='\n')
                          else:
                            self.inline(f'ToAny({params_name}.{prop_name}));', end='\n')
                      case "array":
                        elem_spec = prop_type["element"]
                        self.write('{')
                        with self.indent():
                          self.write('LSPArray array;')
                          self.inline('for (const ', indent=True)
                          self.generate_type_declaration(elem_spec)
                          if is_optional:
                            self.inline(f' &elem : {params_name}.{prop_name}.value()) {{', end='\n')
                          else:
                            self.inline(f' &elem : {params_name}.{prop_name}) {{', end='\n')
                          with self.indent():
                            match elem_spec["kind"]:
                              case "base":
                                self.inline('array.push_back(', indent=True)
                                self.generate_lower_type_name(elem_spec)
                                self.inline('ToAny(elem));', end='\n')
                              case "reference":
                                self.inline('array.push_back(', indent=True)
                                self.generate_lower_type_name(elem_spec)
                                self.inline('ToAny(*elem));', end='\n')
                              case _:
                                raise ValueError(f'Unsupported array type ({elem_spec["kind"]}): {elem_spec}')
                          self.write('}')
                          self.write('std::unique_ptr<LSPAny> any = std::make_unique<LSPAny>();')
                          self.write('(*any) = std::move(array);')
                          self.write(f'object.emplace("{prop_name}", std::move(any));')
                        self.write('}')
                      # case "or" | "literal":
                      #   pass
                      case _:
                        raise ValueError(f'Unsupported property kind ({prop_type["kind"]}): {prop_type}')
                  if is_optional:
                    self.write('}')
                self.newline()
                self.write('messageParams = std::move(object);')
          case _:
            raise ValueError(f'Unsupported parameter type ({params_spec["kind"]}): {params_spec}')
        self.write('return messageParams;')
      self.write('}')
      self.newline()

  def generate_outgoing_request_methods(self: Self) -> None:
    self.write('// ================= //')
    self.write('// Outgoing Requests //')
    self.write('// ================= //')
    self.newline()
    for request_spec in self.schema["requests"]:
      if request_spec["messageDirection"] == "serverToClient":
        self.generate_as_outgoing_params("requestParams", request_spec)
        result_spec = request_spec.get("result", None)
        if result_spec is not None:
          request_method = request_spec["method"]
          request_name = method_to_camel_case(request_method)
          result_name = f'{request_name}Result'
          with self.nested_names_as(deque([result_name])):
            self.generate_any_to_nested_variant(result_spec)

  def generate_outgoing_notification(self: Self) -> None:
    self.write('// ====================== //')
    self.write('// Outgoing Notifications //')
    self.write('// ====================== //')
    self.newline()
    for notification_spec in self.schema["notifications"]:
      if notification_spec["messageDirection"] == "serverToClient":
        self.generate_as_outgoing_params("notificationParams", notification_spec)

  def generate_constructor(self: Self) -> None:
    self.write('LspTransformer::LspTransformer(lsl::Logger &logger)')
    with self.indent():
      self.write(': logger(logger)')
    self.write('{')
    with self.indent():
      self.write('// empty')
    self.write('}')
    self.newline()

  def generate_code(self: Self) -> None:
    print(f'Generating: {self.file_path}')
    self.write('// -----------------------------------------------------------------------------')
    self.write('// NOTE: This file was generated from Microsoft\'s Language Server Protocol (LSP)')
    self.write('// specification. Please do not edit it by hand.')
    self.write('// -----------------------------------------------------------------------------')
    self.newline()
    self.write('#include <cmath>')
    self.write('#include <stdexcept>')
    self.newline()
    self.write('#include <server/specification.h>')
    self.write('#include <server/lsp_exception.h>')
    self.write('#include <server/lsp_transformer.h>')
    self.newline()
    self.write(f'namespace {self.namespace} {{')
    self.newline()
    with self.indent():
      self.generate_constructor()
      self.generate_copy_methods()
      self.generate_enumeration_transforms()
      self.generate_structure_transforms()
      self.generate_type_alias_transforms()
      self.generate_incoming_request_methods()
      self.generate_incoming_notification()
      self.generate_outgoing_request_methods()
      self.generate_outgoing_notification()
    self.write(f'}} // {self.namespace}')

class CPlusPlusLspLanguageServerHeaderGenerator(CPlusPlusFileGenerator):

  def __init__(
      self: Self,
      output_dir: Path,
      schema: Dict[str, Any],
      namespace: str,
      symbols: Dict[str, Tuple[str, Dict[str, Any]]]
  ) -> None:
    specification_source = output_dir / "lsp_language_server.h"
    super().__init__(specification_source, schema, namespace, symbols)

  def generate_incoming_request_handlers(self: Self) -> None:
    self.write('// ================= //')
    self.write('// Incoming Requests //')
    self.write('// ================= //')
    self.newline()
    for request_spec in self.schema["requests"]:
      if request_spec["messageDirection"] == "clientToServer":
        request_method = request_spec["method"]
        request_name = method_to_camel_case(request_method)
        result_name = f'{request_name}Result'
        self.generate_docstring(request_spec.get("documentation", None))
        params_spec = request_spec.get("params", None)
        if params_spec is not None:
          self.write(f'virtual auto {receive_fn(request_method)}(')
          with self.indent(): self.write(f'{params_spec["name"]} &params')
          self.write(f') -> {result_name};')
        else:
          self.write(f'virtual auto {receive_fn(request_method)}() -> {result_name};')
        self.newline()

  def generate_incoming_notification_handlers(self: Self) -> None:
    self.write('// ====================== //')
    self.write('// Incoming Notifications //')
    self.write('// ====================== //')
    self.newline()
    for notification_spec in self.schema["notifications"]:
      if notification_spec["messageDirection"] == "clientToServer":
        notification_method = notification_spec["method"]
        notification_name = method_to_camel_case(notification_method)
        self.generate_docstring(notification_spec.get("documentation", None))
        params_spec = notification_spec.get("params", None)
        if params_spec is not None:
          self.write(f'virtual auto {receive_fn(notification_method)}(')
          with self.indent(): self.write(f'{params_spec["name"]} &params')
          self.write(f') -> void;')
        else:
          self.write(f'virtual auto {receive_fn(notification_method)}() -> void;')
        self.newline()

  def generate_outgoing_request_handlers(self: Self) -> None:
    self.write('// ================= //')
    self.write('// Outgoing Requests //')
    self.write('// ================= //')
    self.newline()
    for request_spec in self.schema["requests"]:
      if request_spec["messageDirection"] == "serverToClient":
        request_method = request_spec["method"]
        request_name = method_to_camel_case(request_method)
        self.generate_docstring(request_spec.get("documentation", None))
        params_spec = request_spec.get("params", None)
        if params_spec is not None:
          self.write(f'virtual auto {send_fn(request_method)}(')
          with self.indent(): self.write(f'{params_spec["name"]} &params')
          self.write(f') -> int;')
        else:
          self.write(f'virtual auto {send_fn(request_method)}() -> int;')
        self.newline()
        self.generate_docstring(request_spec.get("documentation", None))
        result_spec = request_spec.get("result", None)
        if result_spec is not None:
          result_name = f'{request_name}Result'
          self.write(f'virtual auto {receive_fn(request_method)}(')
          with self.indent():
            if result_spec["kind"] == "base":
              self.write(f'{result_name} params')
            else:
              self.write(f'{result_name} &params')
          self.write(f') -> void;')
        else:
          self.write(f'virtual auto {receive_fn(request_method)}() -> void;')
        self.newline()

  def generate_outgoing_notification_handlers(self: Self) -> None:
    self.write('// ====================== //')
    self.write('// Outgoing Notifications //')
    self.write('// ====================== //')
    self.newline()
    for notification_spec in self.schema["notifications"]:
      if notification_spec["messageDirection"] == "serverToClient":
        notification_method = notification_spec["method"]
        notification_name = method_to_camel_case(notification_method)
        self.generate_docstring(notification_spec.get("documentation", None))
        params_spec = notification_spec.get("params", None)
        if params_spec is not None:
          self.write(f'virtual auto {send_fn(notification_method)}(')
          with self.indent(): self.write(f'{params_spec["name"]} &params')
          self.write(f') -> void;')
        else:
          self.write(f'virtual auto {send_fn(notification_method)}() -> void;')
        self.newline()

  def generate_dispatch_methods(self: Self) -> None:
    self.write('auto dispatch(')
    with self.indent():
      self.write('ResponseMessage &response,')
      self.write('RequestMessage &request')
    self.write(') -> void;')
    self.newline()
    self.write('auto dispatch(')
    with self.indent():
      self.write('ResponseMessage &response,')
      self.write('NotificationMessage &notification')
    self.write(') -> void;')
    self.newline()
    self.write('auto dispatch(ResponseMessage &response) -> void;')
    self.newline()

  def generate_prepare(self: Self) -> None:
    self.write('void prepare(')
    with self.indent():
      self.write('std::string &buffer,')
      self.write('const std::string &response')
    self.write(') const override;')
    self.newline()

  def generate_require_message_params(self: Self) -> None:
    self.write('auto requireMessageParams(')
    with self.indent():
      self.write('RequestMessage &request')
    self.write(') const -> MessageParams &;')
    self.newline()
    self.write('auto requireMessageParams(')
    with self.indent():
      self.write('NotificationMessage &notification')
    self.write(') const -> MessageParams &;')
    self.newline()

  def generate_next_send_id(self: Self) -> None:
    self.write('inline auto nextSendId() -> std::size_t {')
    with self.indent():
      self.write('return serialSendId++;')
    self.write('}')
    self.newline()

  def generate_next_request_id(self: Self) -> None:
    self.write('inline auto nextRequestId() -> int {')
    with self.indent():
      self.write('return serialRequestId++;')
    self.write('}')
    self.newline()

  def generate_is_initialized(self: Self) -> None:
    self.write('inline auto isInitialized() const -> bool {')
    with self.indent():
      self.write('return _initialized;')
    self.write('}')
    self.newline()

  def generate_is_shutdown(self: Self) -> None:
    self.write('inline auto isShutdown() const -> bool {')
    with self.indent():
      self.write('return _shutdown;')
    self.write('}')
    self.newline()

  def generate_is_running(self: Self) -> None:
    self.write('inline auto isRunning() const -> bool {')
    with self.indent():
      self.write('return !_shutdown;')
    self.write('}')
    self.newline()

  def generate_code(self: Self) -> None:
    print(f'Generating: {self.file_path}')
    self.write('// -----------------------------------------------------------------------------')
    self.write('// NOTE: This file was generated from Microsoft\'s Language Server Protocol (LSP)')
    self.write('// specification. Please do not edit it by hand.')
    self.write('// -----------------------------------------------------------------------------')
    self.newline()
    self.write('#pragma once')
    self.newline()
    self.write('#include <atomic>')
    self.write('#include <condition_variable>')
    self.write('#include <map>')
    self.write('#include <mutex>')
    self.write('#include <thread>')
    self.newline()
    self.write('#include <server/language_server.h>')
    self.write('#include <server/logger.h>')
    self.write('#include <server/lsp_json_serializer.h>')
    self.write('#include <server/lsp_transformer.h>')
    self.write('#include <server/specification.h>')
    self.newline()
    self.write(f'namespace {self.namespace} {{')
    with self.indent():
      self.write('namespace ls = LCompilers::LLanguageServer;')
      self.write('namespace lsl = LCompilers::LLanguageServer::Logging;')
      self.write('namespace lst = LCompilers::LLanguageServer::Threading;')
      self.newline()
      self.write('class LspLanguageServer : public ls::LanguageServer {')
      self.write('public:')
      with self.indent():
        self.write('LspLanguageServer(')
        with self.indent():
          self.write('ls::MessageQueue &incomingMessages,')
          self.write('ls::MessageQueue &outgoingMessages,')
          self.write('std::size_t numRequestThreads,')
          self.write('std::size_t numWorkerThreads,')
          self.write('lsl::Logger &logger,')
          self.write('const std::string &configSection')
        self.write(');')
        self.newline()
        self.generate_is_initialized();
        self.generate_is_shutdown();
        self.generate_is_running()
        self.write('bool isTerminated() const override;')
      self.write('protected:')
      with self.indent():
        self.write('const std::string configSection;')
        self.write('std::thread listener;')
        self.write('lst::ThreadPool requestPool;')
        self.write('lst::ThreadPool workerPool;')
        self.write('std::atomic_size_t serialSendId = 0;')
        self.write('std::atomic_size_t pendingSendId = 0;')
        self.write('std::condition_variable sent;')
        self.write('std::mutex sentMutex;')
        self.write('LspJsonSerializer serializer;')
        self.write('LspTransformer transformer;')
        self.write('std::unique_ptr<InitializeParams> _initializeParams;')
        self.write('std::atomic_bool _initialized = false;')
        self.write('std::atomic_bool _shutdown = false;')
        self.write('std::atomic_bool _exit = false;')
        self.write('std::atomic_int serialRequestId = 0;')
        self.write('std::map<int, std::string> callbacksById;')
        self.write('std::mutex callbackMutex;')
        self.newline()
        self.generate_next_send_id()
        self.generate_next_request_id()
        self.write('void join() override;')
        self.write('auto listen() -> void;')
        self.write('auto notifySent() -> void;')
        self.write('auto send(')
        with self.indent():
          self.write('const std::string &request,')
          self.write('std::size_t sendId')
        self.write(') -> void;')
        self.write('auto handle(')
        with self.indent():
          self.write('const std::string &request,')
          self.write('std::size_t sendId')
        self.write(') -> void;')
        self.write('auto initializeParams() const -> const InitializeParams &;')
        self.write('auto assertInitialized() -> void;')
        self.write('auto assertRunning() -> void;')
        self.newline()
        self.generate_dispatch_methods()
        self.generate_prepare()
        self.generate_require_message_params()
        self.generate_incoming_request_handlers()
        self.generate_incoming_notification_handlers()
        self.generate_outgoing_request_handlers()
        self.generate_outgoing_notification_handlers()
      self.write('}; // class LspLanguageServer')
      self.newline()
    self.write(f'}} // namespace {self.namespace}')

class CPlusPlusLspLanguageServerSourceGenerator(CPlusPlusFileGenerator):

  def __init__(
      self: Self,
      output_dir: Path,
      schema: Dict[str, Any],
      namespace: str,
      symbols: Dict[str, Tuple[str, Dict[str, Any]]]
  ) -> None:
    specification_source = output_dir / "lsp_language_server.cpp"
    super().__init__(specification_source, schema, namespace, symbols)

  def generate_constructor(self: Self) -> None:
    self.write('LspLanguageServer::LspLanguageServer(')
    with self.indent():
      self.write('ls::MessageQueue &incomingMessages,')
      self.write('ls::MessageQueue &outgoingMessages,')
      self.write('std::size_t numRequestThreads,')
      self.write('std::size_t numWorkerThreads,')
      self.write('lsl::Logger &logger,')
      self.write('const std::string &configSection')
    self.write(') : ls::LanguageServer(')
    self.write('    incomingMessages,')
    self.write('    outgoingMessages,')
    self.write('    logger')
    self.write('  )')
    self.write('  , configSection(configSection)')
    self.write('  , listener([this]() {')
    with self.indent():
      self.write('  listen();')
    self.write('  })')
    self.write('  , requestPool("request", numRequestThreads, logger)')
    self.write('  , workerPool("worker", numWorkerThreads, logger)')
    self.write('  , transformer(logger)')
    self.write('{')
    with self.indent():
      self.write('// empty')
    self.write('}')
    self.newline()

  def generate_join(self: Self) -> None:
    self.write('auto LspLanguageServer::join() -> void {')
    with self.indent():
      self.write('if (listener.joinable()) {')
      with self.indent():
        self.write('listener.join();')
      self.write('}')
      self.write('requestPool.join();')
      self.write('workerPool.join();')
    self.write('}')
    self.newline()

  def generate_listen(self: Self) -> None:
    self.write('auto LspLanguageServer::listen() -> void {')
    with self.indent():
      self.write('try {')
      with self.indent():
        self.write('while (!_exit) {')
        with self.indent():
          self.write('const std::string message = incomingMessages.dequeue();')
          self.write('if (!_exit) {')
          with self.indent():
            self.write('std::size_t sendId = nextSendId();')
            self.write('requestPool.execute([this, message, sendId](')
            with self.indent():
              self.write('const std::string &threadName,')
              self.write('const std::size_t threadId')
            self.write(') {')
            with self.indent():
              self.write('try {')
              with self.indent():
                self.write('handle(message, sendId);')
              self.write('} catch (std::exception &e) {')
              with self.indent():
                self.write('std::unique_lock<std::recursive_mutex> loggerLock(logger.mutex());')
                self.write('logger.error()')
                with self.indent():
                  self.write('<< "[" << threadName << "_" << threadId << "] "')
                  self.write('<< "Failed to handle message: " << message')
                  self.write('<< std::endl;')
                self.write('logger.error()')
                with self.indent():
                  self.write('<< "[" << threadName << "_" << threadId << "] "')
                  self.write('<< "Caught unhandled exception: " << e.what()')
                  self.write('<< std::endl;')
              self.write('}')
            self.write('});')
          self.write('}')
        self.write('}')
      self.write('} catch (std::exception &e) {')
      with self.indent():
        self.write('logger.warn()')
        with self.indent():
          self.write('<< "[LspLanguageServer] Interrupted while dequeuing messages: "')
          self.write('<< e.what()')
          self.write('<< std::endl;')
      self.write('}')
      self.write('logger.debug()')
      with self.indent():
        self.write('<< "[LspLanguageServer] Incoming-message listener terminated."')
        self.write('<< std::endl;')
    self.write('}')
    self.newline()

  def generate_notify_sent(self: Self) -> None:
    self.write('auto LspLanguageServer::notifySent() -> void {')
    with self.indent():
      self.write('++pendingSendId;')
      self.write('{')
      with self.indent():
        self.write('std::unique_lock<std::mutex> sentLock(sentMutex);')
        self.write('sent.notify_all();')
      self.write('}')
    self.write('}')
    self.newline()

  def generate_synchronized_send(self: Self) -> None:
    self.write('auto LspLanguageServer::send(')
    with self.indent():
      self.write('const std::string &message,')
      self.write('std::size_t sendId')
    self.write(') -> void {')
    with self.indent():
      self.write('// -------------------------------------------------------------------------')
      self.write('// NOTE: The LSP spec requires responses to be returned in roughly the same')
      self.write('// order of receipt of their corresponding requests. Some types of responses')
      self.write('// may be returned out-of-order, but in order to support those we will need')
      self.write('// to implement a sort of dependency graph. Without knowledge of their')
      self.write('// dependencies, we must respond to all requests in order of receipt.')
      self.write('// -------------------------------------------------------------------------')
      self.write('{')
      with self.indent():
        self.write('std::unique_lock<std::mutex> sentLock(sentMutex);')
        self.write('sent.wait(sentLock, [this, sendId]{')
        with self.indent():
          self.write('return (pendingSendId == sendId) || _exit;')
        self.write('});')
      self.write('}')
      self.write('if ((pendingSendId == sendId) && !_exit) {')
      with self.indent():
        self.write('ls::LanguageServer::send(message);')
        self.write('notifySent();')
      self.write('}')
    self.write('}')
    self.newline()

  def generate_handle(self: Self) -> None:
    self.write('auto LspLanguageServer::handle(')
    with self.indent():
      self.write('const std::string &message,')
      self.write('std::size_t sendId')
    self.write(') -> void {')
    with self.indent():
      self.write('ResponseMessage response;')
      self.write('try {')
      with self.indent():
        self.write('// The language server protocol always uses “2.0” as the jsonrpc version.')
        self.write('response.jsonrpc = JSON_RPC_VERSION;')
        self.write('response.id = nullptr;')
        self.newline()
        self.write('LspJsonParser parser(message);')
        self.write('std::unique_ptr<LSPAny> document = parser.parse();')
        self.write('LSPAnyType documentType = static_cast<LSPAnyType>(document->index());')
        self.newline()
        self.write(f'if (documentType != LSPAnyType::{rename_enum("object")}) {{')
        with self.indent():
          self.write('// TODO: Add support for batched messages, i.e. multiple messages within')
          self.write('// an array.')
          self.write(f'if (documentType == LSPAnyType::{rename_enum("array")}) {{')
          with self.indent():
            self.write('throw LSP_EXCEPTION(')
            with self.indent():
              self.write('ErrorCodes::INVALID_PARAMS,')
              self.write('"Batched requests are not supported (currently)."')
            self.write(');')
          self.write('}')
          self.write('throw LSP_EXCEPTION(')
          with self.indent():
            self.write('ErrorCodes::INVALID_PARAMS,')
            self.write('"Invalid request message: " + message')
          self.write(');')
        self.write('}')
        self.newline()
        self.write('const LSPObject &object = std::get<LSPObject>(*document);')
        self.write('LSPObject::const_iterator iter;')
        self.newline()
        self.write('if ((iter = object.find("id")) != object.end()) {')
        with self.indent():
          self.write('response.id = transformer.anyToResponseId(*iter->second);')
        self.write('}')
        self.newline()
        self.write('if ((iter = object.find("method")) != object.end()) {')
        with self.indent():
          self.write('const std::string &method =')
          with self.indent():
            self.write('transformer.anyToString(*iter->second);')
          self.write('if (isIncomingRequest(method)) {')
          with self.indent():
            self.write('if (static_cast<ResponseIdType>(response.id.index()) ==')
            with self.indent(2): self.write('ResponseIdType::NULL_TYPE) {')
            with self.indent():
              self.write('throw LSP_EXCEPTION(')
              with self.indent():
                self.write('ErrorCodes::INVALID_PARAMS,')
                self.write('"Missing request method=\\"" + method + "\\" attribute: id"')
              self.write(');')
            self.write('}')
            self.write('std::unique_ptr<RequestMessage> request =')
            with self.indent():
              self.write('transformer.anyToRequestMessage(*document);')
            self.write('response.jsonrpc = request->jsonrpc;')
            self.write('dispatch(response, *request);')
          self.write('} else if (isIncomingNotification(method)) {')
          with self.indent():
            self.write('if (static_cast<ResponseIdType>(response.id.index()) !=')
            with self.indent(2):
              self.write('ResponseIdType::NULL_TYPE) {')
            with self.indent():
              self.write('throw LSP_EXCEPTION(')
              with self.indent():
                self.write('ErrorCodes::INVALID_PARAMS,')
                self.write('"Notification method=\\"" + method + "\\" must not contain the attribute: id"')
              self.write(');')
            self.write('}')
            self.write('std::unique_ptr<NotificationMessage> notification =')
            with self.indent():
              self.write('transformer.anyToNotificationMessage(*document);')
            self.write('response.jsonrpc = notification->jsonrpc;')
            self.write('dispatch(response, *notification);')
          self.write('} else {')
          with self.indent():
            self.write('throw LSP_EXCEPTION(')
            with self.indent():
              self.write('ErrorCodes::INVALID_REQUEST,')
              self.write('"Unsupported method: \\"" + method + "\\""')
            self.write(');')
          self.write('}')
        self.write('} else if ((iter = object.find("result")) != object.end()) {')
        with self.indent():
          self.write('notifySent();')
          self.write('response.result = transformer.copy(iter->second);')
          self.write('dispatch(response);')
          self.write('return;')
        self.write('} else if ((iter = object.find("error")) != object.end()) {')
        with self.indent():
          self.write('notifySent();')
          self.write('response.error = transformer.anyToResponseError(*iter->second);')
          self.write('dispatch(response);')
          self.write('return;')
        self.write("} else {")
        with self.indent():
          self.write('throw LSP_EXCEPTION(')
          with self.indent():
            self.write('ErrorCodes::INVALID_REQUEST,')
            self.write('"Missing required attribute: method"')
          self.write(');')
        self.write('}')
      self.write('} catch (const LspException &e) {')
      with self.indent():
        self.write('logger.error()')
        with self.indent():
          self.write('<< "[" << e.file() << ":" << e.line() << "] "')
          self.write('<< e.what()')
          self.write('<< std::endl;')
        self.write('std::unique_ptr<ResponseError> error =')
        with self.indent():
          self.write('std::make_unique<ResponseError>();')
        self.write(f'switch (static_cast<ErrorCodeType>(e.code().index())) {{')
        self.write('case ErrorCodeType::ERROR_CODES: {')
        with self.indent():
          self.write('ErrorCodes errorCode = std::get<ErrorCodes>(e.code());')
          self.write('error->code = static_cast<int>(errorCode);')
          self.write('break;')
        self.write('}')
        self.write('case ErrorCodeType::LSP_ERROR_CODES: {')
        with self.indent():
          self.write('LSPErrorCodes errorCode =')
          with self.indent():
            self.write('std::get<LSPErrorCodes>(e.code());')
          self.write('error->code = static_cast<int>(errorCode);')
          self.write('break;')
        self.write('}')
        self.write('}')
        self.write('error->message = e.what();')
        self.write('response.error = std::move(error);')
      self.write('} catch (const std::exception &e) {')
      with self.indent():
        self.write('logger.error()')
        with self.indent():
          self.write('<< "Caught unhandled exception: "')
          self.write('<< e.what() << std::endl;')
        self.write('std::unique_ptr<ResponseError> error =')
        with self.indent():
          self.write('std::make_unique<ResponseError>();')
        self.write('error->code = static_cast<int>(ErrorCodes::INTERNAL_ERROR);')
        self.write('error->message =')
        with self.indent():
          self.write('("An unexpected exception occurred. If it continues, "')
          self.write(' "please file a ticket.");')
        self.write('response.error = std::move(error);')
      self.write('}')
      self.write('std::unique_ptr<LSPAny> any =')
      with self.indent():
        self.write('transformer.responseMessageToAny(response);')
      self.write('const std::string message = serializer.serialize(*any);')
      self.write('send(message, sendId);')
    self.write('}')
    self.newline()

  def generate_is_terminated(self: Self) -> None:
    self.write('auto LspLanguageServer::isTerminated() const -> bool {')
    with self.indent():
      self.write('return _exit;')
    self.write('}')
    self.newline()

  def generate_initialize_params(self: Self) -> None:
    self.write('auto LspLanguageServer::initializeParams(')
    self.write(') const -> const InitializeParams & {')
    with self.indent():
      self.write('if (_initializeParams) {')
      with self.indent():
        self.write('return *_initializeParams;')
      self.write('}')
      self.write('throw std::logic_error("Server has not been initialized.");')
    self.write('}')
    self.newline()

  def generate_assert_initialized(self: Self) -> None:
    self.write('auto LspLanguageServer::assertInitialized() -> void{')
    with self.indent():
      self.write('if (!_initialized) {')
      with self.indent():
        self.write('throw LSP_EXCEPTION(')
        with self.indent():
          self.write('ErrorCodes::SERVER_NOT_INITIALIZED,')
          self.write('"Method \\"initialize\\" must be called first."')
        self.write(');')
      self.write('}')
    self.write('}')
    self.newline()

  def generate_assert_running(self: Self) -> None:
    self.write('auto LspLanguageServer::assertRunning() -> void {')
    with self.indent():
      self.write('if (_shutdown) {')
      with self.indent():
        self.write('throw LSP_EXCEPTION(')
        with self.indent():
          self.write('LSPErrorCodes::REQUEST_FAILED,')
          self.write('"Server has shutdown and cannot accept new requests."')
        self.write(');')
      self.write('}')
    self.write('}')
    self.newline()

  def generate_dispatch_request(self: Self) -> None:
    self.write('auto LspLanguageServer::dispatch(')
    with self.indent():
      self.write('ResponseMessage &response,')
      self.write('RequestMessage &request')
    self.write(') -> void {')
    with self.indent():
      self.write('IncomingRequest method;')
      self.write('try {')
      with self.indent():
        self.write('method = incomingRequestByValue(request.method);')
      self.write('} catch (std::invalid_argument &e) {')
      with self.indent():
        self.write('goto invalidMethod;')
      self.write('}')
      self.write('assertRunning();')
      self.write('if (method != IncomingRequest::INITIALIZE) {')
      with self.indent():
        self.write('assertInitialized();')
      self.write('} else {')
      with self.indent():
        self.write('bool expected = false;  // a reference is required')
        self.write('if (!_initialized.compare_exchange_strong(expected, true)) {')
        with self.indent():
          self.write('throw LSP_EXCEPTION(')
          with self.indent():
            self.write('ErrorCodes::INVALID_REQUEST,')
            self.write('"Server may be initialized only once."')
          self.write(');')
        self.write('}')
      self.write('}')
      self.write('switch (method) {')
      for request_spec in self.schema["requests"]:
        if request_spec["messageDirection"] == "clientToServer":
          request_method = request_spec["method"]
          request_name = method_to_camel_case(request_method)
          result_name = f'{request_name}Result'
          self.write(f'case IncomingRequest::{method_to_underscore(request_method)}: {{')
          with self.indent():
            params_spec = request_spec.get("params", None)
            if params_spec is not None:
              is_initialize = request_method == "initialize"
              num_levels = int(is_initialize)
              if is_initialize:
                self.write('try {')
              with self.indent(num_levels):
                self.write('MessageParams &messageParams = requireMessageParams(request);')
                self.write(f'std::unique_ptr<{params_spec["name"]}> requestParams =')
                with self.indent(): self.write(f'transformer.as{request_name}Params(messageParams);')
                self.write(f'{result_name} result =')
                with self.indent(): self.write(f'{receive_fn(request_method)}(*requestParams);')
                self.write(f'response.result =')
                with self.indent():
                  self.write(f'transformer.{lower_first(result_name)}ToAny(result);')
                if is_initialize:
                  self.write('_initializeParams = std::move(requestParams);')
              if is_initialize:
                self.write('} catch (LspException &e) {')
                with self.indent():
                  self.write('bool expected = true;')
                  self.write('if (!_initialized.compare_exchange_strong(expected, false)) {')
                  with self.indent():
                    self.write('throw LSP_EXCEPTION(')
                    with self.indent():
                      self.write('ErrorCodes::INVALID_REQUEST,')
                      self.write('"Server initialization out of sync."')
                    self.write(');')
                  self.write('}')
                  self.write('throw e;')
                self.write('}')
            else:
              self.write(f'{result_name} result = {receive_fn(request_method)}();')
              self.write(f'response.result =')
              with self.indent():
                self.write(f'transformer.{lower_first(result_name)}ToAny(result);')
            self.write('break;')
          self.write('}')
      self.write('default: {')
      self.write('invalidMethod:')
      with self.indent():
        self.write('throw LSP_EXCEPTION(')
        with self.indent():
          self.write('ErrorCodes::METHOD_NOT_FOUND,')
          self.write('"Unsupported request method: \\"" + request.method + "\\""')
        self.write(');')
      self.write('}')
      self.write('}')
    self.write('}')
    self.newline()

  def generate_dispatch_notification(self: Self) -> None:
    self.write('auto LspLanguageServer::dispatch(')
    with self.indent():
      self.write('ResponseMessage &/*response*/,')
      self.write('NotificationMessage &notification')
    self.write(') -> void {')
    with self.indent():
      self.write('IncomingNotification method;')
      self.write('try {')
      with self.indent():
        self.write('method = incomingNotificationByValue(notification.method);')
      self.write('} catch (std::invalid_argument &e) {')
      with self.indent():
        self.write('goto invalidMethod;')
      self.write('}')
      self.write('if (method != IncomingNotification::EXIT) {')
      with self.indent():
        self.write('if (!_initialized) {')
        with self.indent():
          self.write('// Notifications should be dropped, except for the exit notification.')
          self.write('// This will allow the exit of a server without an initialize request.')
          self.write('return;')
        self.write('}')
        self.write('assertRunning();')
      self.write('}')
      self.write('switch (method) {')
      for notification_spec in self.schema["notifications"]:
        if notification_spec["messageDirection"] == "clientToServer":
          notification_method = notification_spec["method"]
          notification_name = method_to_camel_case(notification_method)
          self.write(f'case IncomingNotification::{method_to_underscore(notification_method)}: {{')
          with self.indent():
            params_spec = notification_spec.get("params", None)
            if params_spec is not None:
              self.write('MessageParams &messageParams = requireMessageParams(notification);')
              self.write(f'std::unique_ptr<{params_spec["name"]}> notificationParams =')
              with self.indent(): self.write(f'transformer.as{notification_name}Params(messageParams);')
              self.write(f'{receive_fn(notification_method)}(*notificationParams);')
            else:
              self.write(f'{receive_fn(notification_method)}();')
            self.write('break;')
          self.write('}')
      self.write('default: {')
      self.write('invalidMethod:')
      with self.indent():
        self.write('throw LSP_EXCEPTION(')
        with self.indent():
          self.write('ErrorCodes::METHOD_NOT_FOUND,')
          self.write('"Unsupported notification method: \\"" + notification.method + "\\""')
        self.write(');')
      self.write('}')
      self.write('}')
    self.write('}')
    self.newline()

  def generate_dispatch_response(self: Self) -> None:
    self.write('auto LspLanguageServer::dispatch(ResponseMessage &response) -> void {')
    with self.indent():
      self.write('ResponseIdType responseIdType =')
      with self.indent():
        self.write('static_cast<ResponseIdType>(response.id.index());')
      self.write(f'if (responseIdType != ResponseIdType::{rename_enum("integer")}) {{')
      with self.indent():
        self.write('logger.error()')
        with self.indent():
          self.write('<< "Cannot dispatch response with id of type ResponseIdType::"')
          self.write('<< ResponseIdTypeNames.at(responseIdType)')
          self.write('<< std::endl;')
        self.write('return;')
      self.write('}')
      self.newline()
      self.write('int responseId = std::get<int>(response.id);')
      self.write('std::string method;')
      self.write('{')
      with self.indent():
        self.write('std::unique_lock<std::mutex> callbackLock(callbackMutex);')
        self.write('auto iter = callbacksById.find(responseId);')
        self.write('if (iter != callbacksById.end()) {')
        with self.indent():
          self.write('method = iter->second;')
          self.write('callbacksById.erase(iter);')
        self.write('} else {')
        with self.indent():
          self.write('logger.error() << "Cannot locate request with id: " << responseId << std::endl;')
          self.write('return;')
        self.write('}')
      self.write('}')
      self.newline()
      self.write('OutgoingRequest request;')
      self.write('try {')
      with self.indent():
        self.write('request = outgoingRequestByValue(method);')
      self.write('} catch (std::invalid_argument &e) {')
      with self.indent():
        self.write('goto invalidMethod;')
      self.write('}')
      self.newline()
      self.write('switch (request) {')
      for request_spec in self.schema["requests"]:
        if request_spec["messageDirection"] == "serverToClient":
          request_method = request_spec["method"]
          request_name = method_to_camel_case(request_method)
          result_name = f'{request_name}Result'
          self.write(f'case OutgoingRequest::{method_to_underscore(request_method)}: {{')
          with self.indent():
            result_spec = request_spec.get("result", None)
            if result_spec is not None:
              self.write('if (!response.result.has_value()) {')
              with self.indent():
                self.write(f'logger.error()')
                with self.indent():
                  self.write(f'<< "Missing required attribute for method \\"{request_method}\\": result"')
                  self.write('<< std::endl;')
                self.write('return;')
              self.write('}')
              self.write('std::unique_ptr<LSPAny> &result = response.result.value();')
              symbol_name = result_name
              symbol_spec = result_spec
              symbol_kind = symbol_spec["kind"]
              while (symbol_kind == "reference") and (symbol_name in self.symbols):
                symbol_kind, symbol_spec = self.symbols[symbol_name]
                symbol_name = symbol_spec["name"]
              if symbol_kind == "structure":
                self.write(f'std::unique_ptr<{result_name}> params =')
              else:
                self.write(f'{result_name} params =')
              with self.indent():
                self.write(f'transformer.anyTo{upper_first_char(result_name)}(*result);')
              if symbol_kind == "structure":
                self.write(f'{receive_fn(request_method)}(*params);')
              else:
                self.write(f'{receive_fn(request_method)}(params);')
            else:
              self.write(f'{receive_fn(request_method)}()')
            self.write('break;')
          self.write('}')
      self.write('default: {')
      self.write('invalidMethod:')
      with self.indent():
        self.write(f'logger.error() << "Unsupported request method: \\"" << method << "\\"";')
      self.write('}')
      self.write('}')
    self.write('}')
    self.newline()

  def generate_require_request_params(self: Self) -> None:
    self.write('auto LspLanguageServer::requireMessageParams(')
    with self.indent():
      self.write('RequestMessage &request')
    self.write(') const -> MessageParams & {')
    with self.indent():
      self.write('if (request.params.has_value()) {')
      with self.indent():
        self.write('return request.params.value();')
      self.write('}')
      self.write('throw LSP_EXCEPTION(')
      with self.indent():
        self.write('ErrorCodes::INVALID_PARAMS,')
        self.write('"RequestMessage.params must be defined for method=\\"" + request.method + "\\""')
      self.write(');')
    self.write('}')
    self.newline()

  def generate_require_notification_params(self: Self) -> None:
    self.write('auto LspLanguageServer::requireMessageParams(')
    with self.indent():
      self.write('NotificationMessage &notification')
    self.write(') const -> MessageParams & {')
    with self.indent():
      self.write('if (notification.params.has_value()) {')
      with self.indent():
        self.write('return notification.params.value();')
      self.write('}')
      self.write('throw LSP_EXCEPTION(')
      with self.indent():
        self.write('ErrorCodes::INVALID_PARAMS,')
        self.write('"NotificationMessage.params must be defined for method=\\"" + notification.method + "\\""')
      self.write(');')
    self.write('}')
    self.newline()

  def generate_require_message_params(self: Self) -> None:
    self.generate_require_request_params()
    self.generate_require_notification_params()

  def generate_incoming_request_handlers(self: Self) -> None:
    self.write('// ================= //')
    self.write('// Incoming Requests //')
    self.write('// ================= //')
    self.newline()
    for request_spec in self.schema["requests"]:
      if request_spec["messageDirection"] == "clientToServer":
        request_method = request_spec["method"]
        request_name = method_to_camel_case(request_method)
        result_name = f'{request_name}Result'
        self.write(f'// request: "{request_method}"')
        params_spec = request_spec.get("params", None)
        if params_spec is not None:
          self.write(f'auto LspLanguageServer::{receive_fn(request_method)}(')
          with self.indent(): self.write(f'{params_spec["name"]} &/*params*/')
          self.write(f') -> {result_name} {{')
        else:
          self.write(f'auto LspLanguageServer::{receive_fn(request_method)}() -> {result_name} {{')
        with self.indent():
          match request_method:
            case "initialize":
              self.write('InitializeResult result;')
              self.newline()
              self.write('std::unique_ptr<ServerCapabilities> capabilities =')
              with self.indent():
                self.write('std::make_unique<ServerCapabilities>();')
              self.newline()
              self.write('return result;')
            case "shutdown":
              self.write('bool expected = false;')
              self.write('if (_shutdown.compare_exchange_strong(expected, true)) {')
              with self.indent():
                self.write('{')
                with self.indent():
                  self.write('logger.info() << "Shutting down server." << std::endl;')
                self.write('}')
              self.write('}')
              self.write('return nullptr;')
            case _:
              self.write('throw LSP_EXCEPTION(')
              with self.indent():
                self.write('ErrorCodes::METHOD_NOT_FOUND,')
                self.write(f'"No handler exists for method: \\"{request_method}\\""')
              self.write(');')
        self.write('}')
        self.newline()

  def generate_incoming_notification_handlers(self: Self) -> None:
    self.write('// ====================== //')
    self.write('// Incoming Notifications //')
    self.write('// ====================== //')
    self.newline()
    for notification_spec in self.schema["notifications"]:
      if notification_spec["messageDirection"] == "clientToServer":
        notification_method = notification_spec["method"]
        notification_name = method_to_camel_case(notification_method)
        self.write(f'// notification: "{notification_method}"')
        params_spec = notification_spec.get("params", None)
        if params_spec is not None:
          self.write(f'auto LspLanguageServer::{receive_fn(notification_method)}(')
          with self.indent(): self.write(f'{params_spec["name"]} &/*params*/')
          self.write(f') -> void {{')
        else:
          self.write(f'auto LspLanguageServer::{receive_fn(notification_method)}() -> void {{')
        with self.indent():
          match notification_method:
            case "exit":
              self.write('bool expected = false;')
              self.write('if (_exit.compare_exchange_strong(expected, true)) {')
              with self.indent():
                self.write('{')
                with self.indent():
                  self.write('logger.info() << "Exiting server." << std::endl;')
                self.write('}')
                self.write('expected = false;')
                self.write('if (_shutdown.compare_exchange_strong(expected, true)) {')
                with self.indent():
                  self.write('logger.error()')
                  with self.indent():
                    self.write('<< "Server exited before being notified to shutdown!"')
                    self.write('<< std::endl;')
                self.write('}')
                self.write('incomingMessages.stopNow();')
                self.write('requestPool.stopNow();')
                self.write('workerPool.stopNow();')
                self.write('// TODO: Find a better way to terminate the message stream:')
                self.write('std::cin.putback(\'\\0\');')
                self.write('fclose(stdin);')
              self.write('}')
            case "initialized":
              self.write('// empty')
            case _:
              self.write('throw LSP_EXCEPTION(')
              with self.indent():
                self.write('ErrorCodes::METHOD_NOT_FOUND,')
                self.write(f'"No handler exists for method: \\"{notification_method}\\""')
              self.write(');')
        self.write('}')
        self.newline()

  def generate_outgoing_request_handlers(self: Self) -> None:
    self.write('// ================= //')
    self.write('// Outgoing Requests //')
    self.write('// ================= //')
    self.newline()
    for request_spec in self.schema["requests"]:
      if request_spec["messageDirection"] == "serverToClient":
        request_method = request_spec["method"]
        request_name = method_to_camel_case(request_method)
        self.write(f'// request: "{request_method}"')
        params_spec = request_spec.get("params", None)
        if params_spec is not None:
          self.write(f'auto LspLanguageServer::{send_fn(request_method)}(')
          with self.indent(): self.write(f'{params_spec["name"]} &params')
          self.write(f') -> int {{')
        else:
          self.write(f'auto LspLanguageServer::{send_fn(request_method)}() -> int {{')
        with self.indent():
          self.write('RequestMessage request;')
          self.write('request.jsonrpc = JSON_RPC_VERSION;')
          self.write('int requestId = nextRequestId();')
          self.write('request.id = requestId;')
          self.write('{')
          with self.indent():
            self.write('std::unique_lock<std::mutex> callbackLock(callbackMutex);')
            self.write(f'callbacksById.emplace(requestId, "{request_method}");')
          self.write('}')
          self.write(f'request.method = "{request_method}";')
          if params_spec is not None:
            self.write('request.params = transformer.asMessageParams(params);')
          self.write('std::unique_ptr<LSPAny> any =')
          with self.indent():
            self.write('transformer.requestMessageToAny(request);')
          self.write('const std::string message = serializer.serialize(*any);')
          self.write('ls::LanguageServer::send(message);')
          self.write('return requestId;')
        self.write('}')
        self.newline()
        result_spec = request_spec.get("result", None)
        if result_spec is not None:
          result_name = f'{request_name}Result'
          self.write(f'auto LspLanguageServer::{receive_fn(request_method)}(')
          with self.indent():
            self.inline(result_name, indent=True)
            if result_spec["kind"] == "base":
              self.inline(' /*params*/', end='\n')
            else:
              self.inline(' &/*params*/', end='\n')
          self.write(') -> void {')
        else:
          self.write(f'auto LspLanguageServer::{receive_fn(request_method)}() -> void {{')
        with self.indent():
          self.write(f'logger.warn()')
          with self.indent():
            self.write(f'<< "No handler exists for method: \\"{request_method}\\""')
            self.write(f'<< std::endl;')
        self.write('}')
        self.newline()

  def generate_outgoing_notification_handlers(self: Self) -> None:
    self.write('// ====================== //')
    self.write('// Outgoing Notifications //')
    self.write('// ====================== //')
    self.newline()
    for notification_spec in self.schema["notifications"]:
      if notification_spec["messageDirection"] == "serverToClient":
        notification_method = notification_spec["method"]
        notification_name = method_to_camel_case(notification_method)
        self.write(f'// notification: "{notification_method}"')
        params_spec = notification_spec.get("params", None)
        if params_spec is not None:
          self.write(f'auto LspLanguageServer::{send_fn(notification_method)}(')
          with self.indent(): self.write(f'{params_spec["name"]} &params')
          self.write(f') -> void {{')
        else:
          self.write(f'auto LspLanguageServer::{send_fn(notification_method)}() -> void {{')
        with self.indent():
          self.write('NotificationMessage notification;')
          self.write('notification.jsonrpc = JSON_RPC_VERSION;')
          self.write(f'notification.method = "{notification_method}";')
          if params_spec is not None:
            self.write('notification.params = transformer.asMessageParams(params);')
          self.write('std::unique_ptr<LSPAny> any =')
          with self.indent():
            self.write('transformer.notificationMessageToAny(notification);')
          self.write('const std::string message = serializer.serialize(*any);')
          self.write('ls::LanguageServer::send(message);')
        self.write('}')
        self.newline()

  def generate_prepare(self: Self) -> None:
    self.write('auto LspLanguageServer::prepare(')
    with self.indent():
      self.write('std::string &buffer,')
      self.write('const std::string &response')
    self.write(') const -> void {')
    with self.indent():
      self.write('buffer.append("Content-Type: application/vscode-jsonrpc; charset=utf-8\\r\\n");')
      self.write('buffer.append("Content-Length: ");')
      self.write('buffer.append(std::to_string(response.length()));')
      self.write('buffer.append("\\r\\n");')
      self.write('buffer.append("\\r\\n");')
      self.write('buffer.append(response);')
    self.write('}')
    self.newline()

  def generate_code(self: Self) -> None:
    print(f'Generating: {self.file_path}')
    self.write('// -----------------------------------------------------------------------------')
    self.write('// NOTE: This file was generated from Microsoft\'s Language Server Protocol (LSP)')
    self.write('// specification. Please do not edit it by hand.')
    self.write('// -----------------------------------------------------------------------------')
    self.newline()
    self.write('#include <cctype>')
    self.write('#include <cstdio>')
    self.write('#include <iostream>')
    self.write('#include <stdexcept>')
    self.newline()
    self.write('#include <server/specification.h>')
    self.write('#include <server/lsp_exception.h>')
    self.write('#include <server/lsp_json_parser.h>')
    self.write('#include <server/lsp_language_server.h>')
    self.newline()
    self.write(f'namespace {self.namespace} {{')
    self.newline()
    with self.indent():
      self.generate_constructor()
      self.generate_join()
      self.generate_listen()
      self.generate_notify_sent()
      self.generate_synchronized_send()
      self.generate_handle()
      self.generate_is_terminated()
      self.generate_initialize_params()
      self.generate_assert_initialized()
      self.generate_assert_running();
      self.generate_prepare()
      self.generate_dispatch_request()
      self.generate_dispatch_notification()
      self.generate_dispatch_response()
      self.generate_require_message_params()
      self.generate_incoming_request_handlers()
      self.generate_incoming_notification_handlers()
      self.generate_outgoing_request_handlers()
      self.generate_outgoing_notification_handlers()
    self.write(f'}} // namespace {self.namespace}')

class CPlusPlusCodeGenerator(LspCodeGenerator):
  namespace: str = "LCompilers::LanguageServerProtocol"
  symbols: Dict[str, Tuple[str, Dict[str, Any]]]

  def __init__(self: Self, args: argparse.Namespace) -> None:
    super().__init__(args)
    self.symbols = {}

  def generate_specification_header(self: Self) -> None:
    code_generator = CPlusPlusSpecificationHeaderGenerator(
      self.args.output_dir,
      self.schema,
      self.namespace,
      self.symbols
    )
    with code_generator.open():
      code_generator.generate_code()

  def generate_specification_source(self: Self) -> None:
    code_generator = CPlusPlusSpecificationSourceGenerator(
      self.args.output_dir,
      self.schema,
      self.namespace,
      self.symbols
    )
    with code_generator.open():
      code_generator.generate_code()

  def generate_specification(self: Self) -> None:
    self.generate_specification_header()
    self.generate_specification_source()

  def generate_transformer_header(self: Self) -> None:
    code_generator = CPlusPlusLspTransformerHeaderGenerator(
      self.args.output_dir,
      self.schema,
      self.namespace,
      self.symbols
    )
    with code_generator.open():
      code_generator.generate_code()

  def generate_transformer_source(self: Self) -> None:
    code_generator = CPlusPlusLspTransformerSourceGenerator(
      self.args.output_dir,
      self.schema,
      self.namespace,
      self.symbols
    )
    with code_generator.open():
      code_generator.generate_code()

  def generate_server_header(self: Self) -> None:
    code_generator = CPlusPlusLspLanguageServerHeaderGenerator(
      self.args.output_dir,
      self.schema,
      self.namespace,
      self.symbols
    )
    with code_generator.open():
      code_generator.generate_code()

  def generate_server_source(self: Self) -> None:
    code_generator = CPlusPlusLspLanguageServerSourceGenerator(
      self.args.output_dir,
      self.schema,
      self.namespace,
      self.symbols
    )
    with code_generator.open():
      code_generator.generate_code()

  def generate_transformer(self: Self) -> None:
    self.generate_transformer_header()
    self.generate_transformer_source()

  def generate_server(self: Self) -> None:
    self.generate_server_header()
    self.generate_server_source()

def parse_args() -> argparse.Namespace:
  parser = argparse.ArgumentParser(
    prog="LSPCodeGenerator",
    description="Generates code in a target language from Microsoft's LSP spec (Language Server Protocol).",
  )
  parser.add_argument(
    "-s", "--schema",
    help="Path to the metaModel.json file describing the current LSP spec (e.g. version 3.17).",
    type=argparse.FileType(mode="r", encoding="UTF-8"),
  )
  parser.add_argument(
    "-l", "--target-language",
    help="Target language for the generated code.",
    choices=["c++"],
  )
  parser.add_argument(
    "-o", "--output-dir",
    help="Path to the directory into which to write the generated files.",
    type=Path,
  )
  return parser.parse_args()

def main() -> int:
  args: argparse.Namespace = parse_args()
  match args.target_language:
    case "c++":
      code_generator = CPlusPlusCodeGenerator(args)
    case _:
      raise ValueError(f"Unsupported target language: {args.target_language}")
  code_generator.generate_code()
  return EXIT_SUCCESS

if __name__ == "__main__":
  try:
    exit_code = main()
    sys.exit(exit_code)
  except SystemExit as system_exit:
    if system_exit.code is not None:
      if system_exit.code != 0 and is_debug_enabled():
        traceback.print_exc()
      sys.exit(system_exit.code)
  except:
    print("Rescued unhandled exception:", file=sys.stderr)
    traceback.print_exc()
    sys.exit(EXIT_FAILURE)
