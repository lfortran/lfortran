"""Deterministic, schema-shaped ASR text generators."""

import json
import random


def string(value):
    return json.dumps(value)


def integer_type(kind):
    return f"(Integer :kind {kind})"


def logical_type(kind=4):
    return f"(Logical :kind {kind})"


def symbol_ref(table, name):
    return f"(SymbolRef {table} {string(name)})"


def var(table, name):
    return f"(Var :v {symbol_ref(table, name)})"


def integer_constant(value, kind):
    return (
        f"(IntegerConstant :n {value} "
        f":type {integer_type(kind)} :intboz_type :Decimal)"
    )


def integer_expression(rng, kind):
    left = rng.randint(-8, 8)
    right = rng.randint(-8, 8)
    if rng.choice([False, True]):
        return integer_constant(left, kind)
    operator = rng.choice(["Add", "Sub", "Mul"])
    value = {
        "Add": left + right,
        "Sub": left - right,
        "Mul": left * right,
    }[operator]
    return (
        f"(IntegerBinOp :left {integer_constant(left, kind)} "
        f":op :{operator} :right {integer_constant(right, kind)} "
        f":type {integer_type(kind)} "
        f":value {integer_constant(value, kind)})"
    )


def assignment(table, name, value, realloc=False, move=False):
    return (
        f"(Assignment :target {var(table, name)} :value {value} "
        f":overloaded nil :realloc_lhs {'true' if realloc else 'false'} "
        f":move_allocation {'true' if move else 'false'})"
    )


def integer_variable(table, name, kind):
    return (
        f"(Variable :parent_symtab {table} :name {string(name)} "
        f":dependencies [] :intent :Local :symbolic_value nil :value nil "
        f":storage :Default :type {integer_type(kind)} "
        f":type_declaration nil :abi :Source :access :Public "
        f":presence :Required :value_attr false :target_attr false "
        f":contiguous_attr false :bindc_name nil :is_volatile false "
        f":is_protected false :pass_attr :NotMethod :self_argument nil "
        f":codims [])"
    )


def real_variable(table, name, kind):
    return (
        f"(Variable :parent_symtab {table} :name {string(name)} "
        f":dependencies [] :intent :Local :symbolic_value nil :value nil "
        f":storage :Default :type (Real :kind {kind}) "
        f":type_declaration nil :abi :Source :access :Public "
        f":presence :Required :value_attr false :target_attr false "
        f":contiguous_attr false :bindc_name nil :is_volatile false "
        f":is_protected false :pass_attr :NotMethod :self_argument nil "
        f":codims [])"
    )


def generate_valid(seed):
    rng = random.Random(seed)
    kinds = [1, 2, 4, 8]
    variable_count = rng.randint(1, 4)
    variables = []
    symbols = []
    for index in range(variable_count):
        name = f"x{index}"
        kind = rng.choice(kinds)
        variables.append((name, kind))
        symbols.append(
            f"{string(name)} {integer_variable(1, name, kind)}")

    body = []
    for _ in range(rng.randint(1, 5)):
        name, kind = rng.choice(variables)
        body.append(assignment(
            1, name, integer_expression(rng, kind)))

    if rng.choice([False, True]):
        name, kind = rng.choice(variables)
        body.append(
            f"(If :name nil "
            f":test (LogicalConstant :value true "
            f":type {logical_type()}) "
            f":body [{assignment(1, name, integer_expression(rng, kind))}] "
            f":orelse [])"
        )

    text = (
        "(TranslationUnit :symtab "
        "(SymbolTable :id 0 :symbols {"
        f"\"generated\" (Program :symtab "
        f"(SymbolTable :id 1 :symbols {{{' '.join(symbols)}}}) "
        f":name \"generated\" :dependencies [] :body [{' '.join(body)}])"
        "}) :items [])\n"
    )
    return text, "schema-valid integer program"


def generate_invalid(seed):
    rng = random.Random(seed)
    if rng.choice([False, True]):
        text, _ = generate_valid(seed)
        text = text.replace(
            ":realloc_lhs false", ":realloc_lhs true", 1)
        return text, "schema-invalid nonallocatable realloc_lhs"

    text = (
        "(TranslationUnit :symtab "
        "(SymbolTable :id 0 :symbols {"
        "\"generated\" (Program :symtab "
        "(SymbolTable :id 1 :symbols {"
        f"\"x\" {real_variable(1, 'x', 1)}"
        "}) :name \"generated\" :dependencies [] :body [])"
        "}) :items [])\n"
    )
    return text, "schema-invalid unsupported real kind"


def generate(mode, seed):
    if mode == "schema-valid":
        return generate_valid(seed)
    if mode == "schema-invalid":
        return generate_invalid(seed)
    raise ValueError(f"unknown schema generator mode {mode!r}")
