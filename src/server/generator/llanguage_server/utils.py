import re
from functools import wraps
from typing import Any, Dict, List, Set, Tuple

RE_CAMEL_CASE_TO_UNDERSCORE: re.Pattern = \
    re.compile("(?:([a-z])([A-Z])|([A-Z0-9])([A-Z][a-z]))")
RE_UPPER_FIRST: re.Pattern = \
    re.compile("^(?:([A-Z])([^A-Z])|([A-Z]+?)([A-Z][^A-Z]|$))")

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
def upper_first(name: str) -> str:
    if name == "uinteger":
        return "UInteger"
    if len(name) > 0:
        return name[0].upper() + name[1:]
    return ""

@memoize
def method_to_camel_case(method: str) -> str:
    if method.startswith("$"):
        method = method[1:]
    return "".join(map(upper_first, method.split("/")))

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
    fn_nym = upper_first(fn_nym)
    return f"send{fn_nym}"

@memoize
def receive_fn(method: str) -> str:
    if method.startswith("$/"):
        method = method[2:]
    fn_nym = method.replace(r'/', '_')
    fn_nym = upper_first(fn_nym)
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
