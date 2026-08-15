"""Deterministic, schema-shaped ASR text generators.

The valid generator builds programs that are correct by construction: it
tracks the symbol table each name lives in, the type of every variable, and
the signature of every procedure it calls, so the graphs it produces are
meant to pass initial verification and reach the LLVM backend. Anything it
gets wrong shows up as a verifier rejection rather than as a compiler bug,
so the campaign summary's compile/verify split is the signal that the
generator is still generating useful work.
"""

import json
import random


PROGRAM_SYMTAB = 1
PROCEDURE_SYMTAB = 2

INTEGER_KINDS = [1, 2, 4, 8]
REAL_KINDS = [4, 8]


def string(value):
    return json.dumps(value)


def integer_type(kind):
    return f"(Integer :kind {kind})"


def real_type(kind):
    return f"(Real :kind {kind})"


def logical_type(kind=4):
    return f"(Logical :kind {kind})"


def complex_type(kind):
    return f"(Complex :kind {kind})"


def complex_constant(re, im, kind):
    return (
        f"(ComplexConstant :re {re!r} :im {im!r} "
        f":type {complex_type(kind)})"
    )


def type_of(declaration):
    name, kind = declaration
    if name == "integer":
        return integer_type(kind)
    if name == "real":
        return real_type(kind)
    if name == "complex":
        return complex_type(kind)
    return logical_type(kind)


def symbol_ref(table, name):
    return f"(SymbolRef {table} {string(name)})"


def var(table, name):
    return f"(Var :v {symbol_ref(table, name)})"


def integer_constant(value, kind):
    return (
        f"(IntegerConstant :n {value} "
        f":type {integer_type(kind)} :intboz_type :Decimal)"
    )


def real_constant(value, kind):
    return f"(RealConstant :r {value!r} :type {real_type(kind)})"


def logical_constant(value, kind=4):
    return (
        f"(LogicalConstant :value {'true' if value else 'false'} "
        f":type {logical_type(kind)})"
    )


def integer_expression(rng, kind, names=()):
    if names and rng.random() < 0.4:
        return var(PROGRAM_SYMTAB, rng.choice(names))
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


def real_expression(rng, kind, names=()):
    if names and rng.random() < 0.4:
        return var(PROGRAM_SYMTAB, rng.choice(names))
    left = round(rng.uniform(-8.0, 8.0), 3)
    if rng.choice([False, True]):
        return real_constant(left, kind)
    right = round(rng.uniform(-8.0, 8.0), 3)
    operator = rng.choice(["Add", "Sub", "Mul"])
    return (
        f"(RealBinOp :left {real_constant(left, kind)} "
        f":op :{operator} :right {real_constant(right, kind)} "
        f":type {real_type(kind)} :value nil)"
    )


def logical_expression(rng, integer_names, kind=4):
    # A comparison is the only way this generator produces a logical value
    # from something other than a literal, so it is also what exercises the
    # compare lowering paths.
    if not integer_names or rng.choice([False, True]):
        return logical_constant(rng.choice([False, True]), kind)
    operator = rng.choice(["Eq", "NotEq", "Lt", "LtE", "Gt", "GtE"])
    name = rng.choice(integer_names)
    return (
        f"(IntegerCompare :left {var(PROGRAM_SYMTAB, name)} "
        f":op :{operator} :right {integer_constant(rng.randint(-8, 8), 4)} "
        f":type {logical_type(kind)} :value nil)"
    )


def assignment(table, name, value, realloc=False, move=False):
    return (
        f"(Assignment :target {var(table, name)} :value {value} "
        f":overloaded nil :realloc_lhs {'true' if realloc else 'false'} "
        f":move_allocation {'true' if move else 'false'})"
    )


def array_type(element, lengths):
    dimensions = " ".join(
        f"(dimension :start {integer_constant(1, 4)} "
        f":length {integer_constant(length, 4)})"
        for length in lengths)
    return (
        f"(Array :type {type_of(element)} :dims [{dimensions}] "
        f":physical_type :FixedSizeArray)"
    )


def array_item(table, name, indices, element):
    entries = " ".join(
        f"(array_index :left nil :right {index} :step nil)"
        for index in indices)
    return (
        f"(ArrayItem :v {var(table, name)} :args [{entries}] "
        f":type {type_of(element)} :storage_format :ColMajor :value nil)"
    )


def variable(table, name, declaration, intent="Local"):
    rendered = declaration if isinstance(declaration, str) \
        else type_of(declaration)
    return (
        f"(Variable :parent_symtab {table} :name {string(name)} "
        f":dependencies [] :intent :{intent} :symbolic_value nil :value nil "
        f":storage :Default :type {rendered} "
        f":type_declaration nil :abi :Source :access :Public "
        f":presence :Required :value_attr false :target_attr false "
        f":contiguous_attr false :bindc_name nil :is_volatile false "
        f":is_protected false :pass_attr :NotMethod :self_argument nil "
        f":codims [])"
    )


def integer_variable(table, name, kind):
    return variable(table, name, ("integer", kind))


def real_variable(table, name, kind):
    return variable(table, name, ("real", kind))


def do_loop(rng, index_name, body):
    start = rng.randint(1, 3)
    return (
        f"(DoLoop :name nil :head (do_loop_head "
        f":v {var(PROGRAM_SYMTAB, index_name)} "
        f":start {integer_constant(start, 4)} "
        f":end {integer_constant(start + rng.randint(0, 3), 4)} "
        f":increment nil) "
        f":body [{' '.join(body)}] :orelse [])"
    )


def if_statement(test, body):
    return (
        f"(If :name nil :test {test} "
        f":body [{' '.join(body)}] :orelse [])"
    )


def subroutine(name, parameter, body):
    """A contained subroutine taking one integer argument by value."""
    parameter_name, kind = parameter
    symbols = {
        parameter_name: variable(
            PROCEDURE_SYMTAB, parameter_name, ("integer", kind), "In"),
        "local": variable(
            PROCEDURE_SYMTAB, "local", ("integer", kind)),
    }
    entries = " ".join(
        f"{string(key)} {value}" for key, value in symbols.items())
    return (
        f"(Function :symtab (SymbolTable :id {PROCEDURE_SYMTAB} "
        f":symbols {{{entries}}}) :name {string(name)} "
        f":function_signature (FunctionType "
        f":arg_types [{integer_type(kind)}] :return_var_type nil "
        f":abi :Source :deftype :Implementation :bindc_name nil "
        f":elemental false :pure false :module false :inline false "
        f":static false :restrictions [] :is_restriction false) "
        f":dependencies [] "
        f":args [{var(PROCEDURE_SYMTAB, parameter_name)}] "
        f":body [{' '.join(body)}] :return_var nil :access :Public "
        f":deterministic true :side_effect_free true :module_file nil)"
    )


def function_symbol(name, parameter_name, kind):
    """A contained function returning the same integer kind it takes."""
    symbols = {
        parameter_name: variable(
            PROCEDURE_SYMTAB, parameter_name, ("integer", kind), "In"),
        name: variable(
            PROCEDURE_SYMTAB, name, ("integer", kind), "ReturnVar"),
    }
    entries = " ".join(
        f"{string(key)} {value}" for key, value in symbols.items())
    body = assignment(
        PROCEDURE_SYMTAB, name, var(PROCEDURE_SYMTAB, parameter_name))
    return (
        f"(Function :symtab (SymbolTable :id {PROCEDURE_SYMTAB} "
        f":symbols {{{entries}}}) :name {string(name)} "
        f":function_signature (FunctionType "
        f":arg_types [{integer_type(kind)}] "
        f":return_var_type {integer_type(kind)} "
        f":abi :Source :deftype :Implementation :bindc_name nil "
        f":elemental false :pure false :module false :inline false "
        f":static false :restrictions [] :is_restriction false) "
        f":dependencies [] "
        f":args [{var(PROCEDURE_SYMTAB, parameter_name)}] "
        f":body [{body}] "
        f":return_var {var(PROCEDURE_SYMTAB, name)} :access :Public "
        f":deterministic true :side_effect_free true :module_file nil)"
    )


def function_call(name, argument, kind):
    return (
        f"(FunctionCall :name {symbol_ref(PROGRAM_SYMTAB, name)} "
        f":original_name nil :args [(call_arg :value {argument})] "
        f":type {integer_type(kind)} :value nil :dt nil)"
    )


def subroutine_call(name, argument):
    return (
        f"(SubroutineCall :name {symbol_ref(PROGRAM_SYMTAB, name)} "
        f":original_name nil :args [(call_arg :value {argument})] "
        f":dt nil :strict_bounds_checking false)"
    )


def translation_unit(symbols, body):
    entries = " ".join(
        f"{string(key)} {value}" for key, value in symbols.items())
    return (
        "(TranslationUnit :symtab "
        "(SymbolTable :id 0 :symbols {"
        f"\"generated\" (Program :symtab "
        f"(SymbolTable :id {PROGRAM_SYMTAB} :symbols {{{entries}}}) "
        f":name \"generated\" :dependencies [] :body [{' '.join(body)}])"
        "}) :items [])\n"
    )


def generate_valid(seed):
    rng = random.Random(seed)
    symbols = {}
    integers = []
    reals = []
    logicals = []

    for index in range(rng.randint(1, 3)):
        name = f"i{index}"
        kind = rng.choice(INTEGER_KINDS)
        integers.append((name, kind))
        symbols[name] = integer_variable(PROGRAM_SYMTAB, name, kind)
    for index in range(rng.randint(0, 2)):
        name = f"r{index}"
        kind = rng.choice(REAL_KINDS)
        reals.append((name, kind))
        symbols[name] = real_variable(PROGRAM_SYMTAB, name, kind)
    if rng.choice([False, True]):
        name = "l0"
        logicals.append((name, 4))
        symbols[name] = variable(PROGRAM_SYMTAB, name, ("logical", 4))

    # A do loop needs an index variable of the default integer kind, and the
    # same variable must not be handed to statements expecting another kind.
    loop_index = "idx"
    symbols[loop_index] = integer_variable(PROGRAM_SYMTAB, loop_index, 4)

    arrays = []
    for index in range(rng.randint(0, 2)):
        name = f"a{index}"
        if rng.choice([False, True]):
            element = ("integer", rng.choice(INTEGER_KINDS))
        else:
            element = ("real", rng.choice(REAL_KINDS))
        lengths = [rng.randint(1, 4) for _ in range(rng.choice([1, 1, 2]))]
        arrays.append((name, element, lengths))
        symbols[name] = variable(
            PROGRAM_SYMTAB, name, array_type(element, lengths))

    complexes = []
    if rng.choice([False, True]):
        name = "c0"
        kind = rng.choice(REAL_KINDS)
        complexes.append((name, kind))
        symbols[name] = variable(PROGRAM_SYMTAB, name, ("complex", kind))

    integer_names_by_kind = {}
    for name, kind in integers:
        integer_names_by_kind.setdefault(kind, []).append(name)

    def integer_statement():
        name, kind = rng.choice(integers)
        names = integer_names_by_kind.get(kind, [])
        return assignment(
            PROGRAM_SYMTAB, name, integer_expression(rng, kind, names))

    def real_statement():
        name, kind = rng.choice(reals)
        return assignment(
            PROGRAM_SYMTAB, name, real_expression(rng, kind))

    def logical_statement():
        name, kind = rng.choice(logicals)
        return assignment(
            PROGRAM_SYMTAB, name,
            logical_expression(rng, integer_names_by_kind.get(4, []), kind))

    def element_expression(element):
        name, kind = element
        if name == "real":
            return real_expression(rng, kind)
        return integer_expression(
            rng, kind, integer_names_by_kind.get(kind, []))

    def bounded_indices(lengths):
        # Indices stay within the declared bounds so a generated program is
        # well defined at runtime, not merely well formed.
        return [integer_constant(rng.randint(1, length), 4)
                for length in lengths]

    def array_statement():
        name, element, lengths = rng.choice(arrays)
        target = array_item(
            PROGRAM_SYMTAB, name, bounded_indices(lengths), element)
        return (
            f"(Assignment :target {target} "
            f":value {element_expression(element)} "
            f":overloaded nil :realloc_lhs false :move_allocation false)"
        )

    def array_read_statement():
        name, element, lengths = rng.choice(arrays)
        element_name, kind = element
        pool = integers if element_name == "integer" else reals
        scalars = [n for n, k in pool if k == kind]
        if not scalars:
            return array_statement()
        return assignment(
            PROGRAM_SYMTAB, rng.choice(scalars),
            array_item(
                PROGRAM_SYMTAB, name, bounded_indices(lengths), element))

    def complex_statement():
        name, kind = rng.choice(complexes)
        return assignment(
            PROGRAM_SYMTAB, name,
            complex_constant(
                round(rng.uniform(-4.0, 4.0), 3),
                round(rng.uniform(-4.0, 4.0), 3), kind))

    choices = [integer_statement]
    if reals:
        choices.append(real_statement)
    if logicals:
        choices.append(logical_statement)
    if arrays:
        choices.append(array_statement)
        choices.append(array_read_statement)
    if complexes:
        choices.append(complex_statement)

    body = [rng.choice(choices)() for _ in range(rng.randint(1, 5))]

    if rng.choice([False, True]):
        body.append(do_loop(
            rng, loop_index,
            [rng.choice(choices)() for _ in range(rng.randint(1, 2))]))

    if rng.choice([False, True]):
        body.append(if_statement(
            logical_expression(rng, integer_names_by_kind.get(4, [])),
            [rng.choice(choices)()]))

    description = "schema-valid integer program"
    if rng.choice([False, True]):
        # A contained procedure exercises a second scope, a call signature,
        # and the actual-versus-formal argument agreement the verifier checks.
        # Only one is generated per program: both would claim the same symbol
        # table id.
        name, kind = rng.choice(integers)
        if rng.choice([False, True]):
            symbols["helper"] = subroutine(
                "helper", ("arg", kind),
                [assignment(
                    PROCEDURE_SYMTAB, "local", var(PROCEDURE_SYMTAB, "arg"))])
            body.append(subroutine_call("helper", var(PROGRAM_SYMTAB, name)))
            description = "schema-valid program with a contained subroutine"
        else:
            symbols["helper"] = function_symbol("helper", "arg", kind)
            body.append(assignment(
                PROGRAM_SYMTAB, name,
                function_call("helper", var(PROGRAM_SYMTAB, name), kind)))
            description = "schema-valid program with a contained function"

    return translation_unit(symbols, body), description


def generate_invalid(seed):
    rng = random.Random(seed)
    if rng.choice([False, True]):
        text, _ = generate_valid(seed)
        text = text.replace(
            ":realloc_lhs false", ":realloc_lhs true", 1)
        return text, "schema-invalid nonallocatable realloc_lhs"

    text = translation_unit(
        {"x": real_variable(PROGRAM_SYMTAB, "x", 1)}, [])
    return text, "schema-invalid unsupported real kind"


def generate(mode, seed):
    if mode == "schema-valid":
        return generate_valid(seed)
    if mode == "schema-invalid":
        return generate_invalid(seed)
    raise ValueError(f"unknown schema generator mode {mode!r}")
