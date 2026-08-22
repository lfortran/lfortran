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


GLOBAL_SYMTAB = 0
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


def deferred_array_type(element, rank=1):
    dimensions = " ".join(
        "(dimension :start nil :length nil)" for _ in range(rank))
    return (
        f"(Array :type {type_of(element)} :dims [{dimensions}] "
        f":physical_type :DescriptorArray)"
    )


def array_item(table, name, indices, element):
    entries = " ".join(
        f"(array_index :left nil :right {index} :step nil)"
        for index in indices)
    return (
        f"(ArrayItem :v {var(table, name)} :args [{entries}] "
        f":type {type_of(element)} :storage_format :ColMajor :value nil)"
    )


def wrap_type(rendered, allocatable=False, pointer=False):
    if allocatable:
        rendered = f"(Allocatable :type {rendered})"
    if pointer:
        rendered = f"(Pointer :type {rendered})"
    return rendered


def variable(table, name, declaration, intent="Local", presence="Required",
             allocatable=False, pointer=False, type_declaration=None):
    rendered = declaration if isinstance(declaration, str) \
        else type_of(declaration)
    rendered = wrap_type(rendered, allocatable=allocatable, pointer=pointer)
    return (
        f"(Variable :parent_symtab {table} :name {string(name)} "
        f":dependencies [] :intent :{intent} :symbolic_value nil :value nil "
        f":storage :Default :type {rendered} "
        f":type_declaration {type_declaration or 'nil'} "
        f":abi :Source :access :Public "
        f":presence :{presence} :value_attr false :target_attr false "
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
    return procedure(
        name, False,
        [dummy("arg", ("integer", kind), "In")],
        body if body else [assignment(
            PROCEDURE_SYMTAB, "local", var(PROCEDURE_SYMTAB, "arg"))],
        extra_locals=[("local", ("integer", kind))])


def dummy(name, declaration, intent="In", presence="Required",
          allocatable=False, pointer=False, type_declaration=None):
    return {
        "name": name,
        "declaration": declaration,
        "intent": intent,
        "presence": presence,
        "allocatable": allocatable,
        "pointer": pointer,
        "type_declaration": type_declaration,
        "type": wrap_type(
            declaration if isinstance(declaration, str)
            else type_of(declaration),
            allocatable=allocatable, pointer=pointer),
    }


def procedure(name, returns, dummies, body, extra_locals=None,
              symtab=PROCEDURE_SYMTAB, deftype="Implementation"):
    """A subroutine or function with an explicit dummy list."""
    symbols = {}
    for dummy_arg in dummies:
        symbols[dummy_arg["name"]] = variable(
            symtab, dummy_arg["name"], dummy_arg["declaration"],
            dummy_arg["intent"], dummy_arg["presence"],
            dummy_arg["allocatable"], dummy_arg["pointer"],
            dummy_arg.get("type_declaration"))
    for local_name, declaration in extra_locals or []:
        symbols[local_name] = variable(symtab, local_name, declaration)
    return_type = "nil"
    return_var = "nil"
    if returns:
        # `returns` is either True, meaning "return what the first dummy
        # declares", or a declaration of its own.
        explicit = returns is not True
        ret_decl = returns if explicit else (
            dummies[0]["declaration"] if dummies else ("integer", 4))
        symbols[name] = variable(symtab, name, ret_decl, "ReturnVar")
        return_type = type_of(ret_decl) if isinstance(ret_decl, tuple) \
            else ret_decl
        return_var = var(symtab, name)
        if not body:
            if explicit:
                body = [assignment(
                    symtab, name, integer_constant(0, ret_decl[1]))]
            elif dummies:
                body = [assignment(
                    symtab, name, var(symtab, dummies[0]["name"]))]
            else:
                body = []
    arg_types = " ".join(dummy_arg["type"] for dummy_arg in dummies)
    args = " ".join(var(symtab, dummy_arg["name"])
                    for dummy_arg in dummies)
    entries = " ".join(
        f"{string(key)} {value}" for key, value in symbols.items())
    return (
        f"(Function :symtab (SymbolTable :id {symtab} "
        f":symbols {{{entries}}}) :name {string(name)} "
        f":function_signature (FunctionType "
        f":arg_types [{arg_types}] :return_var_type {return_type} "
        f":abi :Source :deftype :{deftype} :bindc_name nil "
        f":elemental false :pure false :module false :inline false "
        f":static false :restrictions [] :is_restriction false) "
        f":dependencies [] "
        f":args [{args}] "
        f":body [{' '.join(body or [])}] "
        f":return_var {return_var} :access :Public "
        f":deterministic true :side_effect_free true :module_file nil "
        f":link_name nil)"
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
        f":deterministic true :side_effect_free true :module_file nil "
        f":link_name nil)"
    )


def function_call(name, argument, kind):
    return (
        f"(FunctionCall :name {symbol_ref(PROGRAM_SYMTAB, name)} "
        f":original_name nil :args [(call_arg :value {argument})] "
        f":type {integer_type(kind)} :value nil :dt nil)"
    )


def subroutine_call(name, argument):
    return call_stmt(name, [argument])


def call_args(values):
    return " ".join(
        "(call_arg :value nil)" if value is None
        else f"(call_arg :value {value})"
        for value in values)


def call_stmt(name, arguments):
    return (
        f"(SubroutineCall :name {symbol_ref(PROGRAM_SYMTAB, name)} "
        f":original_name nil :args [{call_args(arguments)}] "
        f":dt nil :strict_bounds_checking false)"
    )


def function_call_args(name, arguments, result_type):
    return (
        f"(FunctionCall :name {symbol_ref(PROGRAM_SYMTAB, name)} "
        f":original_name nil :args [{call_args(arguments)}] "
        f":type {result_type} :value nil :dt nil)"
    )


def scope(symtab_id, symbols):
    entries = " ".join(
        f"{string(key)} {value}" for key, value in symbols.items())
    return f"(SymbolTable :id {symtab_id} :symbols {{{entries}}})"


def program(symtab_id, name, symbols, body):
    return (
        f"(Program :symtab {scope(symtab_id, symbols)} :name {string(name)} "
        f":dependencies [] :body [{' '.join(body)}])"
    )


def global_unit(symbols, items=()):
    """A TranslationUnit whose global scope holds `symbols` verbatim."""
    return (
        f"(TranslationUnit :symtab {scope(GLOBAL_SYMTAB, symbols)} "
        f":items [{' '.join(items)}])\n"
    )


def translation_unit(symbols, body):
    return global_unit({
        "generated": program(PROGRAM_SYMTAB, "generated", symbols, body),
    })


def module(symtab_id, name, symbols, dependencies=(), parent_module=None,
           has_submodules=False):
    names = " ".join(string(item) for item in dependencies)
    return (
        f"(Module :symtab {scope(symtab_id, symbols)} :name {string(name)} "
        f":parent_module {string(parent_module) if parent_module else 'nil'} "
        f":dependencies [{names}] :loaded_from_mod false "
        f":intrinsic false "
        f":has_submodules {'true' if has_submodules else 'false'})"
    )


def external_symbol(table, name, target_table, target_name, module_name,
                    original_name=None):
    return (
        f"(ExternalSymbol :parent_symtab {table} :name {string(name)} "
        f":external {symbol_ref(target_table, target_name)} "
        f":module_name {string(module_name)} :scope_names [] "
        f":original_name {string(original_name or target_name)} "
        f":access :Public)"
    )


def function_type(arg_types, return_type=None, deftype="Interface"):
    return (
        f"(FunctionType :arg_types [{' '.join(arg_types)}] "
        f":return_var_type {return_type or 'nil'} :abi :Source "
        f":deftype :{deftype} :bindc_name nil :elemental false :pure false "
        f":module false :inline false :static false :restrictions [] "
        f":is_restriction false)"
    )


def class_type():
    """The type of a `class(...)` entity; the symbol lives in the variable."""
    return (
        "(StructType :data_member_types [] :member_function_types [] "
        ":is_cstruct false :is_unlimited_polymorphic false)"
    )


def struct_signature():
    return (
        "(StructType :data_member_types [] :member_function_types [] "
        ":is_cstruct true :is_unlimited_polymorphic false)"
    )


def struct_method(table, name, proc, proc_name=None, deferred=False,
                  nopass=False):
    return (
        f"(StructMethodDeclaration :parent_symtab {table} "
        f":name {string(name)} :self_argument nil "
        f":proc_name {string(proc_name or name)} :proc {proc} "
        f":abi :Source :is_deferred {'true' if deferred else 'false'} "
        f":is_nopass {'true' if nopass else 'false'})"
    )


def struct(symtab_id, name, methods, parent=None, abstract=False):
    return (
        f"(Struct :symtab {scope(symtab_id, methods)} :name {string(name)} "
        f":struct_signature {struct_signature()} :dependencies [] "
        f":members [] :member_functions [] :abi :Source :access :Public "
        f":is_packed false :is_abstract {'true' if abstract else 'false'} "
        f":is_sequence false :initializers [] :alignment nil "
        f":parent {parent or 'nil'} :kind_params [])"
    )


MODULE_SYMTAB = 1
BASE_STRUCT_SYMTAB = 2
BASE_PROC_SYMTAB = 3
DERIVED_STRUCT_SYMTAB = 4
DERIVED_PROC_SYMTAB = 5
OO_PROGRAM_SYMTAB = 6


def self_dummy(struct_table, struct_name, intent="InOut"):
    return dummy(
        "self", class_type(), intent,
        type_declaration=symbol_ref(struct_table, struct_name))


def type_bound_unit(base_dummies, derived_dummies, deferred=True,
                    base_returns=False, derived_returns=False,
                    base_nopass=False, derived_nopass=False,
                    override_name="meth"):
    """One module with an abstract base type and an extending type.

    Both types declare a type-bound procedure named `meth`; the extending
    type's declaration overrides the base one, so their two procedures must
    have conforming interfaces.
    """
    base_args = ([] if base_nopass
                 else [self_dummy(MODULE_SYMTAB, "base")]) + list(base_dummies)
    derived_args = ([] if derived_nopass
                    else [self_dummy(MODULE_SYMTAB, "derived")]) \
        + list(derived_dummies)
    symbols = {
        "base": struct(
            BASE_STRUCT_SYMTAB, "base",
            {"meth": struct_method(
                BASE_STRUCT_SYMTAB, "meth",
                symbol_ref(MODULE_SYMTAB, "base_meth"),
                proc_name="base_meth", deferred=deferred,
                nopass=base_nopass)},
            abstract=deferred),
        "base_meth": procedure(
            "base_meth", base_returns, base_args, [],
            symtab=BASE_PROC_SYMTAB,
            deftype="Interface" if deferred else "Implementation"),
        "derived": struct(
            DERIVED_STRUCT_SYMTAB, "derived",
            {override_name: struct_method(
                DERIVED_STRUCT_SYMTAB, override_name,
                symbol_ref(MODULE_SYMTAB, "derived_meth"),
                proc_name="derived_meth", nopass=derived_nopass)},
            parent=symbol_ref(MODULE_SYMTAB, "base")),
        "derived_meth": procedure(
            "derived_meth", derived_returns, derived_args, [],
            symtab=DERIVED_PROC_SYMTAB),
    }
    return global_unit({
        "m": module(MODULE_SYMTAB, "m", symbols),
        "generated": program(OO_PROGRAM_SYMTAB, "generated", {}, []),
    })


def generate_valid(seed):
    rng = random.Random(seed)
    if rng.random() < 0.15:
        return rng.choice(VALID_WRAPPER_BUILDERS)(rng)
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
        flavor = rng.choice([
            "plain", "optional", "allocatable", "pointer",
            "intent_out", "two_arg_optional",
        ])
        if flavor == "plain" and rng.choice([False, True]):
            symbols["helper"] = function_symbol("helper", "arg", kind)
            body.append(assignment(
                PROGRAM_SYMTAB, name,
                function_call("helper", var(PROGRAM_SYMTAB, name), kind)))
            description = "schema-valid program with a contained function"
        else:
            dummies, actuals, extra_locals, desc = valid_call_shape(
                rng, flavor, name, kind, symbols)
            symbols["helper"] = procedure(
                "helper", False, dummies, [], extra_locals)
            body.append(call_stmt("helper", actuals))
            description = desc

    return translation_unit(symbols, body), description


def valid_call_shape(rng, flavor, name, kind, symbols):
    extra_locals = [("local", ("integer", kind))]
    if flavor == "optional":
        return (
            [dummy("arg", ("integer", kind), "In", "Optional")],
            [var(PROGRAM_SYMTAB, name)] if rng.choice([False, True]) else [],
            extra_locals,
            "schema-valid optional argument call",
        )
    if flavor == "allocatable":
        actual = f"alloc_{name}"
        symbols[actual] = variable(
            PROGRAM_SYMTAB, actual, ("integer", kind),
            allocatable=True)
        return (
            [dummy("arg", ("integer", kind), "InOut", allocatable=True)],
            [var(PROGRAM_SYMTAB, actual)],
            extra_locals,
            "schema-valid allocatable argument call",
        )
    if flavor == "pointer":
        actual = f"ptr_{name}"
        symbols[actual] = variable(
            PROGRAM_SYMTAB, actual, ("integer", kind), pointer=True)
        return (
            [dummy("arg", ("integer", kind), "InOut", pointer=True)],
            [var(PROGRAM_SYMTAB, actual)],
            extra_locals,
            "schema-valid pointer argument call",
        )
    if flavor == "intent_out":
        return (
            [dummy("arg", ("integer", kind), "Out")],
            [var(PROGRAM_SYMTAB, name)],
            extra_locals,
            "schema-valid intent(out) argument call",
        )
    if flavor == "two_arg_optional":
        second = dummy("opt", ("integer", kind), "In", "Optional")
        actuals = [var(PROGRAM_SYMTAB, name)]
        if rng.choice([False, True]):
            actuals.append(var(PROGRAM_SYMTAB, name))
        return (
            [dummy("arg", ("integer", kind), "In"), second],
            actuals,
            extra_locals,
            "schema-valid required-plus-optional call",
        )
    return (
        [dummy("arg", ("integer", kind), "In")],
        [var(PROGRAM_SYMTAB, name)],
        extra_locals,
        "schema-valid program with a contained subroutine",
    )


def generate_invalid(seed):
    rng = random.Random(seed)
    builders = [
        invalid_realloc_lhs,
        invalid_real_kind,
        invalid_call_kind,
        invalid_call_family,
        invalid_call_plain_to_allocatable,
        invalid_call_plain_to_pointer,
        invalid_call_omit_required,
        invalid_call_extra_arg,
        invalid_call_scalar_to_array,
        invalid_call_array_to_scalar,
        invalid_call_intent_in_to_out,
        invalid_call_optional_wrong_kind,
        invalid_function_call_kind,
        invalid_call_plain_array_to_allocatable_array,
    ] + OVERRIDE_BUILDERS + REFERENCE_BUILDERS + CALL_SITE_BUILDERS \
        + PROGRAM_UNIT_BUILDERS + TYPE_REACH_BUILDERS
    return rng.choice(builders)(rng)


def invalid_realloc_lhs(rng):
    text, _ = generate_valid(rng.randrange(1 << 30))
    text = text.replace(":realloc_lhs false", ":realloc_lhs true", 1)
    return text, "schema-invalid nonallocatable realloc_lhs"


def invalid_real_kind(_rng):
    text = translation_unit(
        {"x": real_variable(PROGRAM_SYMTAB, "x", 1)}, [])
    return text, "schema-invalid unsupported real kind"


def _mismatch_program(dummies, actuals, actual_vars, description,
                      returns=False):
    symbols = dict(actual_vars)
    symbols["helper"] = procedure("helper", returns, dummies, [])
    if returns:
        result = "res"
        symbols[result] = variable(
            PROGRAM_SYMTAB, result, dummies[0]["declaration"]
            if dummies else ("integer", 4))
        result_type = dummies[0]["type"] if dummies else integer_type(4)
        body = [assignment(
            PROGRAM_SYMTAB, result,
            function_call_args("helper", actuals, result_type))]
    else:
        body = [call_stmt("helper", actuals)]
    return translation_unit(symbols, body), description


def invalid_call_kind(_rng):
    return _mismatch_program(
        [dummy("arg", ("integer", 4), "In")],
        [var(PROGRAM_SYMTAB, "x")],
        {"x": integer_variable(PROGRAM_SYMTAB, "x", 2)},
        "schema-invalid call kind mismatch")


def invalid_call_family(_rng):
    return _mismatch_program(
        [dummy("arg", ("integer", 4), "In")],
        [var(PROGRAM_SYMTAB, "x")],
        {"x": real_variable(PROGRAM_SYMTAB, "x", 4)},
        "schema-invalid call family mismatch")


def valid_call_allocatable_to_plain(_rng):
    return _mismatch_program(
        [dummy("arg", ("integer", 4), "In")],
        [var(PROGRAM_SYMTAB, "x")],
        {"x": variable(
            PROGRAM_SYMTAB, "x", ("integer", 4), allocatable=True)},
        "schema-valid allocatable actual to nonallocatable dummy")


def invalid_call_plain_to_allocatable(_rng):
    return _mismatch_program(
        [dummy("arg", ("integer", 4), "InOut", allocatable=True)],
        [var(PROGRAM_SYMTAB, "x")],
        {"x": integer_variable(PROGRAM_SYMTAB, "x", 4)},
        "schema-invalid nonallocatable actual to allocatable dummy")


def valid_call_pointer_to_plain(_rng):
    return _mismatch_program(
        [dummy("arg", ("integer", 4), "In")],
        [var(PROGRAM_SYMTAB, "x")],
        {"x": variable(
            PROGRAM_SYMTAB, "x", ("integer", 4), pointer=True)},
        "schema-valid pointer actual to nonpointer dummy")


def invalid_call_plain_to_pointer(_rng):
    return _mismatch_program(
        [dummy("arg", ("integer", 4), "InOut", pointer=True)],
        [var(PROGRAM_SYMTAB, "x")],
        {"x": integer_variable(PROGRAM_SYMTAB, "x", 4)},
        "schema-invalid nonpointer actual to pointer dummy")


def invalid_call_omit_required(_rng):
    return _mismatch_program(
        [dummy("arg", ("integer", 4), "In")],
        [],
        {},
        "schema-invalid omitted required argument")


def invalid_call_extra_arg(_rng):
    return _mismatch_program(
        [dummy("arg", ("integer", 4), "In")],
        [var(PROGRAM_SYMTAB, "x"), var(PROGRAM_SYMTAB, "x")],
        {"x": integer_variable(PROGRAM_SYMTAB, "x", 4)},
        "schema-invalid extra actual argument")


def invalid_call_scalar_to_array(_rng):
    dummy_type = array_type(("integer", 4), [3])
    return _mismatch_program(
        [dummy("arg", dummy_type, "In")],
        [var(PROGRAM_SYMTAB, "x")],
        {"x": integer_variable(PROGRAM_SYMTAB, "x", 4)},
        "schema-invalid scalar actual to array dummy")


def invalid_call_array_to_scalar(_rng):
    return _mismatch_program(
        [dummy("arg", ("integer", 4), "In")],
        [var(PROGRAM_SYMTAB, "x")],
        {"x": variable(
            PROGRAM_SYMTAB, "x", array_type(("integer", 4), [3]))},
        "schema-invalid array actual to scalar dummy")


def invalid_call_intent_in_to_out(_rng):
    return _mismatch_program(
        [dummy("arg", ("integer", 4), "Out")],
        [var(PROGRAM_SYMTAB, "x")],
        {"x": variable(PROGRAM_SYMTAB, "x", ("integer", 4), "In")},
        "schema-invalid intent(in) actual to intent(out) dummy")


def invalid_call_optional_wrong_kind(_rng):
    return _mismatch_program(
        [dummy("arg", ("integer", 4), "In", "Optional")],
        [var(PROGRAM_SYMTAB, "x")],
        {"x": integer_variable(PROGRAM_SYMTAB, "x", 8)},
        "schema-invalid optional actual kind mismatch")


def invalid_call_plain_array_to_allocatable_array(_rng):
    dummy_type = deferred_array_type(("integer", 4), 1)
    actual_type = array_type(("integer", 4), [3])
    return _mismatch_program(
        [dummy("arg", dummy_type, "InOut", allocatable=True)],
        [var(PROGRAM_SYMTAB, "x")],
        {"x": variable(PROGRAM_SYMTAB, "x", actual_type)},
        "schema-invalid plain array to allocatable array dummy")


def valid_call_allocatable_array_to_plain_array(_rng):
    dummy_type = array_type(("integer", 4), [3])
    actual_type = deferred_array_type(("integer", 4), 1)
    return _mismatch_program(
        [dummy("arg", dummy_type, "In")],
        [var(PROGRAM_SYMTAB, "x")],
        {"x": variable(
            PROGRAM_SYMTAB, "x", actual_type, allocatable=True)},
        "schema-valid allocatable array to plain array dummy")


# Passing an allocatable or a pointer actual to a plain dummy is valid
# Fortran -- only the dummy's own wrapper constrains the actual -- so these
# exercise the argument lowering paths rather than a verifier rule.
VALID_WRAPPER_BUILDERS = [
    valid_call_allocatable_to_plain,
    valid_call_pointer_to_plain,
    valid_call_allocatable_array_to_plain_array,
]


def invalid_function_call_kind(_rng):
    return _mismatch_program(
        [dummy("arg", ("integer", 4), "In")],
        [var(PROGRAM_SYMTAB, "x")],
        {"x": integer_variable(PROGRAM_SYMTAB, "x", 2)},
        "schema-invalid function-call kind mismatch",
        returns=True)


# --- non-conforming type-bound procedure overrides -----------------------
#
# An extending type may only override a binding with a procedure whose
# interface matches the one it overrides. A call dispatched through the parent
# type is compiled against the parent's interface, so a mismatch is a call to a
# procedure with a signature the call site never agreed to.

INTEGER = ("integer", 4)


def override_case(derived_dummies, description, **kwargs):
    return type_bound_unit(
        [dummy("a", INTEGER, "InOut")], derived_dummies, **kwargs), description


def invalid_override_extra_argument(_rng):
    return override_case(
        [dummy("i1", INTEGER, "InOut"), dummy("a", INTEGER, "InOut")],
        "schema-invalid override with an extra argument")


def invalid_override_missing_argument(_rng):
    return type_bound_unit(
        [dummy("a", INTEGER, "InOut"), dummy("b", INTEGER, "InOut")],
        [dummy("a", INTEGER, "InOut")],
    ), "schema-invalid override with a missing argument"


def invalid_override_argument_family(_rng):
    return override_case(
        [dummy("a", ("real", 8), "InOut")],
        "schema-invalid override argument family mismatch")


def invalid_override_argument_kind(_rng):
    return override_case(
        [dummy("a", ("integer", 8), "InOut")],
        "schema-invalid override argument kind mismatch")


def invalid_override_argument_intent(_rng):
    return override_case(
        [dummy("a", INTEGER, "In")],
        "schema-invalid override argument intent mismatch")


def invalid_override_argument_allocatable(_rng):
    return type_bound_unit(
        [dummy("a", INTEGER, "InOut", allocatable=True)],
        [dummy("a", INTEGER, "InOut")],
    ), "schema-invalid override argument allocatable mismatch"


def invalid_override_argument_optional(_rng):
    return override_case(
        [dummy("a", INTEGER, "InOut", presence="Optional")],
        "schema-invalid override argument optional mismatch")


def invalid_override_argument_rank(_rng):
    return override_case(
        [dummy("a", array_type(INTEGER, [3]), "InOut")],
        "schema-invalid override argument rank mismatch")


def invalid_override_returns_a_value(_rng):
    return override_case(
        [dummy("a", INTEGER, "InOut")],
        "schema-invalid subroutine overridden by a function",
        derived_returns=INTEGER)


def invalid_override_returns_nothing(_rng):
    return override_case(
        [dummy("a", INTEGER, "InOut")],
        "schema-invalid function overridden by a subroutine",
        base_returns=INTEGER)


def invalid_override_nopass(_rng):
    return override_case(
        [dummy("a", INTEGER, "InOut")],
        "schema-invalid override nopass mismatch",
        derived_nopass=True)


OVERRIDE_BUILDERS = [
    invalid_override_extra_argument,
    invalid_override_missing_argument,
    invalid_override_argument_family,
    invalid_override_argument_kind,
    invalid_override_argument_intent,
    invalid_override_argument_allocatable,
    invalid_override_argument_optional,
    invalid_override_argument_rank,
    invalid_override_returns_a_value,
    invalid_override_returns_nothing,
    invalid_override_nopass,
]


# --- references to symbols that are missing, out of scope, or wrong ------
#
# A frontend that loses track of a symbol -- a name never imported, a name
# resolved in the wrong scope, a name that turned out to be something other
# than a procedure -- hands the rest of the compiler a graph like one of
# these. Every one of them must be a diagnostic, never a crash.

MODULE_PROC_SYMTAB = 7
SECOND_MODULE_SYMTAB = 8


def module_unit(module_symbols, program_symbols, body, dependencies=()):
    return global_unit({
        "m": module(MODULE_SYMTAB, "m", module_symbols, dependencies),
        "generated": program(
            OO_PROGRAM_SYMTAB, "generated", program_symbols, body),
    })


def module_procedure(name="helper"):
    return procedure(name, False, [dummy("arg", INTEGER, "In")], [],
                     [("local", INTEGER)], symtab=MODULE_PROC_SYMTAB)


def invalid_reference_callee_local(_rng):
    """Read a variable that lives in the callee's scope, not the caller's."""
    return module_unit(
        {"helper": module_procedure()},
        {"x": integer_variable(OO_PROGRAM_SYMTAB, "x", 4)},
        [assignment(OO_PROGRAM_SYMTAB, "x",
                    var(MODULE_PROC_SYMTAB, "local"))],
    ), "schema-invalid read of a callee local"


def invalid_reference_module_not_used(_rng):
    """Calling a module procedure that the program never imported."""
    return module_unit(
        {"helper": module_procedure()},
        {"x": integer_variable(OO_PROGRAM_SYMTAB, "x", 4)},
        [f"(SubroutineCall :name {symbol_ref(MODULE_SYMTAB, 'helper')} "
         f":original_name nil "
         f":args [(call_arg :value {var(OO_PROGRAM_SYMTAB, 'x')})] "
         f":dt nil :strict_bounds_checking false)"],
    ), "schema-invalid call to an unimported module procedure"


def invalid_reference_sibling_module(_rng):
    return global_unit({
        "m": module(MODULE_SYMTAB, "m",
                    {"y": integer_variable(MODULE_SYMTAB, "y", 4)}),
        "generated": program(
            OO_PROGRAM_SYMTAB, "generated",
            {"x": integer_variable(OO_PROGRAM_SYMTAB, "x", 4)},
            [assignment(OO_PROGRAM_SYMTAB, "x", var(MODULE_SYMTAB, "y"))]),
    }), "schema-invalid read of a sibling module variable"


def invalid_reference_call_a_variable(_rng):
    return translation_unit(
        {"x": integer_variable(PROGRAM_SYMTAB, "x", 4)},
        [f"(SubroutineCall :name {symbol_ref(PROGRAM_SYMTAB, 'x')} "
         f":original_name nil :args [] :dt nil "
         f":strict_bounds_checking false)"],
    ), "schema-invalid call to a variable"


def invalid_reference_var_is_a_program(_rng):
    return translation_unit(
        {"x": integer_variable(PROGRAM_SYMTAB, "x", 4)},
        [assignment(PROGRAM_SYMTAB, "x",
                    var(GLOBAL_SYMTAB, "generated"))],
    ), "schema-invalid read of a program symbol"


def invalid_reference_binding_is_a_variable(_rng):
    return module_unit(
        {"base": struct(BASE_STRUCT_SYMTAB, "base",
                        {"meth": struct_method(
                            BASE_STRUCT_SYMTAB, "meth",
                            symbol_ref(MODULE_SYMTAB, "notaproc"))}),
         "notaproc": integer_variable(MODULE_SYMTAB, "notaproc", 4)},
        {}, [],
    ), "schema-invalid type-bound procedure naming a variable"


def invalid_reference_binding_is_a_type(_rng):
    return module_unit(
        {"base": struct(BASE_STRUCT_SYMTAB, "base",
                        {"meth": struct_method(
                            BASE_STRUCT_SYMTAB, "meth",
                            symbol_ref(MODULE_SYMTAB, "other"))}),
         "other": struct(DERIVED_STRUCT_SYMTAB, "other", {})},
        {}, [],
    ), "schema-invalid type-bound procedure naming a derived type"


def invalid_reference_external_unknown_module(_rng):
    return module_unit(
        {"helper": module_procedure()},
        {"helper": external_symbol(
            OO_PROGRAM_SYMTAB, "helper", MODULE_SYMTAB, "helper",
            "nosuchmodule")},
        [],
    ), "schema-invalid import from an unknown module"


def invalid_reference_external_wrong_module(_rng):
    return global_unit({
        "m": module(MODULE_SYMTAB, "m", {"helper": module_procedure()}),
        "m2": module(SECOND_MODULE_SYMTAB, "m2", {}),
        "generated": program(
            OO_PROGRAM_SYMTAB, "generated",
            {"helper": external_symbol(
                OO_PROGRAM_SYMTAB, "helper", MODULE_SYMTAB, "helper", "m2")},
            []),
    }), "schema-invalid import naming the wrong module"


def invalid_reference_external_original_name(_rng):
    return module_unit(
        {"helper": module_procedure()},
        {"helper": external_symbol(
            OO_PROGRAM_SYMTAB, "helper", MODULE_SYMTAB, "helper", "m",
            original_name="missing")},
        [],
    ), "schema-invalid import with a mismatched original name"


def invalid_reference_external_is_a_program(_rng):
    return module_unit(
        {"helper": module_procedure()},
        {"p": external_symbol(
            OO_PROGRAM_SYMTAB, "p", GLOBAL_SYMTAB, "generated", "m")},
        [],
    ), "schema-invalid import of a program"


def invalid_reference_block_call_is_a_function(_rng):
    return translation_unit(
        {"helper": procedure("helper", False, [], [], [])},
        [f"(BlockCall :label -1 "
         f":m {symbol_ref(PROGRAM_SYMTAB, 'helper')})"],
    ), "schema-invalid block call naming a procedure"


def invalid_reference_associate_call_is_a_function(_rng):
    return translation_unit(
        {"helper": procedure("helper", False, [], [], [])},
        [f"(AssociateBlockCall :m {symbol_ref(PROGRAM_SYMTAB, 'helper')})"],
    ), "schema-invalid associate block call naming a procedure"


def invalid_reference_generic_specific(_rng):
    return module_unit(
        {"helper": module_procedure(),
         "notaproc": integer_variable(MODULE_SYMTAB, "notaproc", 4),
         "gen": f"(GenericProcedure :parent_symtab {MODULE_SYMTAB} "
                f":name \"gen\" "
                f":procs [{symbol_ref(MODULE_SYMTAB, 'notaproc')}] "
                f":access :Public)"},
        {}, [],
    ), "schema-invalid generic procedure naming a variable"


def invalid_reference_operator_specific(_rng):
    return module_unit(
        {"notaproc": integer_variable(MODULE_SYMTAB, "notaproc", 4),
         "op": f"(CustomOperator :parent_symtab {MODULE_SYMTAB} :name \"op\" "
               f":procs [{symbol_ref(MODULE_SYMTAB, 'notaproc')}] "
               f":access :Public)"},
        {}, [],
    ), "schema-invalid custom operator naming a variable"


def invalid_reference_struct_parent(_rng):
    return module_unit(
        {"helper": module_procedure(),
         "base": struct(BASE_STRUCT_SYMTAB, "base", {},
                        parent=symbol_ref(MODULE_SYMTAB, "helper"))},
        {}, [],
    ), "schema-invalid derived type extending a procedure"


def invalid_reference_type_declaration(_rng):
    return module_unit(
        {"helper": module_procedure()},
        {"x": variable(OO_PROGRAM_SYMTAB, "x", class_type(),
                       type_declaration=symbol_ref(MODULE_SYMTAB, "helper"))},
        [],
    ), "schema-invalid derived type declared by a procedure"


def invalid_reference_namelist_member(_rng):
    return module_unit(
        {"helper": module_procedure()},
        {"nml": f"(Namelist :parent_symtab {OO_PROGRAM_SYMTAB} "
                f":group_name \"nml\" "
                f":var_list [{symbol_ref(MODULE_SYMTAB, 'helper')}])"},
        [],
    ), "schema-invalid namelist naming a procedure"


def invalid_reference_dummy_not_local(_rng):
    """A procedure whose dummy argument is the caller's variable."""
    helper = procedure("helper", False, [dummy("arg", INTEGER, "In")], [], [])
    helper = helper.replace(
        f":args [{var(PROCEDURE_SYMTAB, 'arg')}]",
        f":args [{var(PROGRAM_SYMTAB, 'x')}]", 1)
    return translation_unit(
        {"x": integer_variable(PROGRAM_SYMTAB, "x", 4), "helper": helper},
        [],
    ), "schema-invalid dummy argument declared by the caller"


def invalid_reference_result_not_local(_rng):
    helper = procedure("helper", True, [dummy("arg", INTEGER, "In")], [], [])
    helper = helper.replace(
        f":return_var {var(PROCEDURE_SYMTAB, 'helper')}",
        f":return_var {var(PROGRAM_SYMTAB, 'x')}", 1)
    return translation_unit(
        {"x": integer_variable(PROGRAM_SYMTAB, "x", 4), "helper": helper},
        [],
    ), "schema-invalid result variable declared by the caller"


def invalid_reference_struct_member(_rng):
    return module_unit(
        {"base": struct(BASE_STRUCT_SYMTAB, "base", {}).replace(
            ":members []", ":members [\"missing\"]", 1)},
        {}, [],
    ), "schema-invalid derived type listing an undeclared member"


def invalid_reference_type_declaration_scope(_rng):
    return global_unit({
        "m": module(MODULE_SYMTAB, "m",
                    {"base": struct(BASE_STRUCT_SYMTAB, "base", {})}),
        "generated": program(
            OO_PROGRAM_SYMTAB, "generated",
            {"x": variable(
                OO_PROGRAM_SYMTAB, "x", class_type(),
                type_declaration=symbol_ref(MODULE_SYMTAB, "base"))},
            []),
    }), "schema-invalid derived type declared from an unimported module"


REFERENCE_BUILDERS = [
    invalid_reference_struct_member,
    invalid_reference_type_declaration_scope,
    invalid_reference_generic_specific,
    invalid_reference_operator_specific,
    invalid_reference_struct_parent,
    invalid_reference_type_declaration,
    invalid_reference_namelist_member,
    invalid_reference_dummy_not_local,
    invalid_reference_result_not_local,
    invalid_reference_callee_local,
    invalid_reference_module_not_used,
    invalid_reference_sibling_module,
    invalid_reference_call_a_variable,
    invalid_reference_var_is_a_program,
    invalid_reference_binding_is_a_variable,
    invalid_reference_binding_is_a_type,
    invalid_reference_external_unknown_module,
    invalid_reference_external_wrong_module,
    invalid_reference_external_original_name,
    invalid_reference_external_is_a_program,
    invalid_reference_block_call_is_a_function,
    invalid_reference_associate_call_is_a_function,
]


# --- call sites that disagree with the procedure they call ---------------
#
# Separate compilation is only worth anything if a call is checked against
# the interface it claims to call. These are the shapes that get past a
# frontend that never compares the two.

def method_call_unit(formal, actual, actual_args=None):
    """A program calling `base%meth` with an actual the binding never took."""
    module_symbols = {
        "base": struct(
            BASE_STRUCT_SYMTAB, "base",
            {"meth": struct_method(
                BASE_STRUCT_SYMTAB, "meth",
                symbol_ref(MODULE_SYMTAB, "base_meth"),
                proc_name="base_meth")},
            parent=None),
        "base_meth": procedure(
            "base_meth", False,
            [self_dummy(MODULE_SYMTAB, "base"), dummy("a", formal, "In")],
            [], symtab=BASE_PROC_SYMTAB),
    }
    program_symbols = {
        "base": external_symbol(
            OO_PROGRAM_SYMTAB, "base", MODULE_SYMTAB, "base", "m"),
        "meth": external_symbol(
            OO_PROGRAM_SYMTAB, "meth", BASE_STRUCT_SYMTAB, "meth", "m"),
        "obj": variable(
            OO_PROGRAM_SYMTAB, "obj", class_type(),
            type_declaration=symbol_ref(OO_PROGRAM_SYMTAB, "base")),
        "x": variable(OO_PROGRAM_SYMTAB, "x", actual),
    }
    if actual_args is None:
        actual_args = [var(OO_PROGRAM_SYMTAB, "obj"),
                       var(OO_PROGRAM_SYMTAB, "x")]
    body = [
        f"(SubroutineCall :name {symbol_ref(OO_PROGRAM_SYMTAB, 'meth')} "
        f":original_name nil :args [{call_args(actual_args)}] "
        f":dt {var(OO_PROGRAM_SYMTAB, 'obj')} "
        f":strict_bounds_checking false)"
    ]
    return global_unit({
        "m": module(MODULE_SYMTAB, "m", module_symbols),
        "generated": program(
            OO_PROGRAM_SYMTAB, "generated", program_symbols, body),
    })


def invalid_method_call_kind(_rng):
    return method_call_unit(INTEGER, ("integer", 8)), \
        "schema-invalid method call argument kind mismatch"


def invalid_method_call_family(_rng):
    return method_call_unit(INTEGER, ("real", 8)), \
        "schema-invalid method call argument family mismatch"


def invalid_method_call_rank(_rng):
    # An array actual for a scalar dummy. The other direction is sequence
    # association, which an explicit-shape dummy allows.
    return method_call_unit(INTEGER, array_type(INTEGER, [3])), \
        "schema-invalid method call argument rank mismatch"


def invalid_call_function_as_subroutine(_rng):
    return module_unit(
        {"helper": procedure(
            "helper", True, [dummy("arg", INTEGER, "In")], [],
            symtab=MODULE_PROC_SYMTAB)},
        {"helper": external_symbol(
            OO_PROGRAM_SYMTAB, "helper", MODULE_SYMTAB, "helper", "m"),
         "x": integer_variable(OO_PROGRAM_SYMTAB, "x", 4)},
        [f"(SubroutineCall "
         f":name {symbol_ref(OO_PROGRAM_SYMTAB, 'helper')} "
         f":original_name nil "
         f":args [(call_arg :value {var(OO_PROGRAM_SYMTAB, 'x')})] "
         f":dt nil :strict_bounds_checking false)"],
    ), "schema-invalid call statement naming a function"


def invalid_call_result_type(_rng):
    return module_unit(
        {"helper": procedure(
            "helper", True, [dummy("arg", INTEGER, "In")], [],
            symtab=MODULE_PROC_SYMTAB)},
        {"helper": external_symbol(
            OO_PROGRAM_SYMTAB, "helper", MODULE_SYMTAB, "helper", "m"),
         "x": real_variable(OO_PROGRAM_SYMTAB, "x", 8),
         "y": integer_variable(OO_PROGRAM_SYMTAB, "y", 4)},
        [assignment(
            OO_PROGRAM_SYMTAB, "x",
            f"(FunctionCall "
            f":name {symbol_ref(OO_PROGRAM_SYMTAB, 'helper')} "
            f":original_name nil "
            f":args [(call_arg :value {var(OO_PROGRAM_SYMTAB, 'y')})] "
            f":type {real_type(8)} :value nil :dt nil)")],
    ), "schema-invalid function call result type mismatch"


TAKER_SYMTAB = 9


def procedure_argument_unit(formal_signature, actual_returns):
    """Pass a module procedure to a dummy declared `procedure(iface)`."""
    module_symbols = {
        "impl": procedure(
            "impl", actual_returns, [dummy("a", INTEGER, "In")], [],
            symtab=MODULE_PROC_SYMTAB),
        "taker": procedure(
            "taker", False,
            [dummy("fp", formal_signature, "In")], [], symtab=TAKER_SYMTAB),
    }
    program_symbols = {
        "taker": external_symbol(
            OO_PROGRAM_SYMTAB, "taker", MODULE_SYMTAB, "taker", "m"),
        "impl": external_symbol(
            OO_PROGRAM_SYMTAB, "impl", MODULE_SYMTAB, "impl", "m"),
    }
    body = [
        f"(SubroutineCall :name {symbol_ref(OO_PROGRAM_SYMTAB, 'taker')} "
        f":original_name nil "
        f":args [(call_arg :value {var(OO_PROGRAM_SYMTAB, 'impl')})] "
        f":dt nil :strict_bounds_checking false)"
    ]
    return global_unit({
        "m": module(MODULE_SYMTAB, "m", module_symbols),
        "generated": program(
            OO_PROGRAM_SYMTAB, "generated", program_symbols, body),
    })


def invalid_procedure_argument_result(_rng):
    return procedure_argument_unit(
        function_type([integer_type(4)]), INTEGER,
    ), "schema-invalid function passed to a subroutine dummy"


def invalid_procedure_argument_type(_rng):
    return procedure_argument_unit(
        function_type([real_type(8)]), False,
    ), "schema-invalid procedure argument interface type mismatch"


def invalid_struct_member_of_other_type(_rng):
    struct_type = ("(StructType :data_member_types [(Integer :kind 4)] "
                   ":member_function_types [] :is_cstruct true "
                   ":is_unlimited_polymorphic false)")
    module_symbols = {
        "base": struct(
            BASE_STRUCT_SYMTAB, "base",
            {"i": integer_variable(BASE_STRUCT_SYMTAB, "i", 4)}).replace(
                ":members []", ":members [\"i\"]", 1),
        "other": struct(
            DERIVED_STRUCT_SYMTAB, "other",
            {"j": integer_variable(DERIVED_STRUCT_SYMTAB, "j", 4)}).replace(
                ":members []", ":members [\"j\"]", 1),
    }
    program_symbols = {
        "base": external_symbol(
            OO_PROGRAM_SYMTAB, "base", MODULE_SYMTAB, "base", "m"),
        "obj": variable(
            OO_PROGRAM_SYMTAB, "obj", struct_type,
            type_declaration=symbol_ref(OO_PROGRAM_SYMTAB, "base")),
        "x": integer_variable(OO_PROGRAM_SYMTAB, "x", 4),
    }
    body = [assignment(
        OO_PROGRAM_SYMTAB, "x",
        f"(StructInstanceMember :v {var(OO_PROGRAM_SYMTAB, 'obj')} "
        f":m {symbol_ref(DERIVED_STRUCT_SYMTAB, 'j')} "
        f":type {integer_type(4)} :value nil)")]
    return global_unit({
        "m": module(MODULE_SYMTAB, "m", module_symbols),
        "generated": program(
            OO_PROGRAM_SYMTAB, "generated", program_symbols, body),
    }), "schema-invalid component of an unrelated derived type"


# --- interfaces a whole program unit has to honour -----------------------
#
# These are the promises a caller in another file is compiled against: the
# interface a submodule implements, the bindings a concrete type must supply,
# and the shapes the compiler itself calls.

SUBMODULE_SYMTAB = 10
SUBMODULE_PROC_SYMTAB = 11


def submodule_unit(declared_dummies, implemented_dummies):
    """A module publishing an interface and a submodule implementing it."""
    interface = procedure(
        "impl", False, declared_dummies, [], symtab=MODULE_PROC_SYMTAB,
        deftype="Interface")
    body = procedure(
        "impl", False, implemented_dummies, [], symtab=SUBMODULE_PROC_SYMTAB)
    return global_unit({
        "m": module(MODULE_SYMTAB, "m", {"impl": interface},
                    has_submodules=True),
        "s": module(SUBMODULE_SYMTAB, "s", {"impl": body}, parent_module="m"),
        "generated": program(OO_PROGRAM_SYMTAB, "generated", {}, []),
    })


def invalid_module_procedure_argument_count(_rng):
    return submodule_unit(
        [dummy("a", INTEGER, "In")],
        [dummy("a", INTEGER, "In"), dummy("b", INTEGER, "In")],
    ), "schema-invalid separate module procedure argument count"


def invalid_module_procedure_argument_type(_rng):
    return submodule_unit(
        [dummy("a", INTEGER, "In")], [dummy("a", ("real", 8), "In")],
    ), "schema-invalid separate module procedure argument type"


def invalid_deferred_binding_not_overridden(_rng):
    """A concrete type inheriting a binding with no body."""
    symbols = {
        "base": struct(
            BASE_STRUCT_SYMTAB, "base",
            {"meth": struct_method(
                BASE_STRUCT_SYMTAB, "meth",
                symbol_ref(MODULE_SYMTAB, "base_meth"),
                proc_name="base_meth", deferred=True, nopass=True)},
            abstract=True),
        "base_meth": procedure(
            "base_meth", False, [dummy("a", INTEGER, "In")], [],
            symtab=BASE_PROC_SYMTAB, deftype="Interface"),
        "derived": struct(
            DERIVED_STRUCT_SYMTAB, "derived", {},
            parent=symbol_ref(MODULE_SYMTAB, "base")),
    }
    return module_unit(symbols, {}, []), \
        "schema-invalid unoverridden deferred binding"


def invalid_abstract_type_variable(_rng):
    struct_type = ("(StructType :data_member_types [] "
                   ":member_function_types [] :is_cstruct true "
                   ":is_unlimited_polymorphic false)")
    return module_unit(
        {"base": struct(BASE_STRUCT_SYMTAB, "base", {}, abstract=True)},
        {"base": external_symbol(
            OO_PROGRAM_SYMTAB, "base", MODULE_SYMTAB, "base", "m"),
         "x": variable(
             OO_PROGRAM_SYMTAB, "x", struct_type,
             type_declaration=symbol_ref(OO_PROGRAM_SYMTAB, "base"))},
        [],
    ), "schema-invalid entity of an abstract type"


def invalid_final_procedure(_rng):
    return module_unit(
        {"base": struct(BASE_STRUCT_SYMTAB, "base", {}).replace(
            ":member_functions []", ":member_functions [\"cleanup\"]", 1),
         "cleanup": procedure(
             "cleanup", False,
             [dummy("self", INTEGER, "InOut"), dummy("extra", INTEGER, "In")],
             [], symtab=MODULE_PROC_SYMTAB)},
        {}, [],
    ), "schema-invalid final procedure signature"


def invalid_procedure_pointer_association(_rng):
    pointer_type = f"(Pointer :type {function_type([integer_type(4)])})"
    return module_unit(
        {"impl": procedure(
            "impl", False, [dummy("a", ("real", 8), "In")], [],
            symtab=MODULE_PROC_SYMTAB)},
        {"impl": external_symbol(
            OO_PROGRAM_SYMTAB, "impl", MODULE_SYMTAB, "impl", "m"),
         "p1": variable(OO_PROGRAM_SYMTAB, "p1", pointer_type)},
        [f"(Associate :target {var(OO_PROGRAM_SYMTAB, 'p1')} "
         f":value {var(OO_PROGRAM_SYMTAB, 'impl')})"],
    ), "schema-invalid procedure pointer association"


PROGRAM_UNIT_BUILDERS = [
    invalid_module_procedure_argument_count,
    invalid_module_procedure_argument_type,
    invalid_deferred_binding_not_overridden,
    invalid_abstract_type_variable,
    invalid_final_procedure,
    invalid_procedure_pointer_association,
]


CALL_SITE_BUILDERS = [
    invalid_procedure_argument_result,
    invalid_procedure_argument_type,
    invalid_struct_member_of_other_type,
    invalid_method_call_kind,
    invalid_method_call_family,
    invalid_method_call_rank,
    invalid_call_function_as_subroutine,
    invalid_call_result_type,
]


# --- types and pointers that cannot hold what they are given -------------
#
# A polymorphic entity reaches a type only through its declared type, a
# sequence type fixes a layout nothing may extend, a pointer needs something
# it may legally alias, and every element of one array constructor lands in
# one array.

def invalid_select_type_unrelated_guard(_rng):
    struct_type = ("(StructType :data_member_types [] "
                   ":member_function_types [] :is_cstruct false "
                   ":is_unlimited_polymorphic false)")
    module_symbols = {
        "base": struct(BASE_STRUCT_SYMTAB, "base", {}),
        "other": struct(DERIVED_STRUCT_SYMTAB, "other", {}),
    }
    program_symbols = {
        "base": external_symbol(
            OO_PROGRAM_SYMTAB, "base", MODULE_SYMTAB, "base", "m"),
        "other": external_symbol(
            OO_PROGRAM_SYMTAB, "other", MODULE_SYMTAB, "other", "m"),
        "b": variable(
            OO_PROGRAM_SYMTAB, "b", struct_type, allocatable=True,
            type_declaration=symbol_ref(OO_PROGRAM_SYMTAB, "base")),
    }
    body = [
        f"(SelectType :selector {var(OO_PROGRAM_SYMTAB, 'b')} "
        f":assoc_name nil "
        f":body [(TypeStmtName "
        f":sym {symbol_ref(OO_PROGRAM_SYMTAB, 'other')} :body [])] "
        f":default [])"
    ]
    return global_unit({
        "m": module(MODULE_SYMTAB, "m", module_symbols),
        "generated": program(
            OO_PROGRAM_SYMTAB, "generated", program_symbols, body),
    }), "schema-invalid type guard on an unrelated type"


def invalid_allocate_unrelated_type(_rng):
    struct_type = ("(StructType :data_member_types [] "
                   ":member_function_types [] :is_cstruct false "
                   ":is_unlimited_polymorphic false)")
    concrete_type = ("(StructType :data_member_types [] "
                     ":member_function_types [] :is_cstruct true "
                     ":is_unlimited_polymorphic false)")
    module_symbols = {
        "base": struct(BASE_STRUCT_SYMTAB, "base", {}),
        "other": struct(DERIVED_STRUCT_SYMTAB, "other", {}),
    }
    program_symbols = {
        "base": external_symbol(
            OO_PROGRAM_SYMTAB, "base", MODULE_SYMTAB, "base", "m"),
        "other": external_symbol(
            OO_PROGRAM_SYMTAB, "other", MODULE_SYMTAB, "other", "m"),
        "b": variable(
            OO_PROGRAM_SYMTAB, "b", struct_type, allocatable=True,
            type_declaration=symbol_ref(OO_PROGRAM_SYMTAB, "base")),
    }
    body = [
        f"(Allocate :args [(alloc_arg :a {var(OO_PROGRAM_SYMTAB, 'b')} "
        f":dims [] :codims [] :len_expr nil "
        f":sym_subclass {symbol_ref(OO_PROGRAM_SYMTAB, 'other')} "
        f":type {concrete_type})] "
        f":stat nil :errmsg nil :source nil)"
    ]
    return global_unit({
        "m": module(MODULE_SYMTAB, "m", module_symbols),
        "generated": program(
            OO_PROGRAM_SYMTAB, "generated", program_symbols, body),
    }), "schema-invalid allocation of an unrelated type"


def invalid_sequence_type_extended(_rng):
    base = struct(BASE_STRUCT_SYMTAB, "base", {}).replace(
        ":is_sequence false", ":is_sequence true", 1)
    return module_unit(
        {"base": base,
         "derived": struct(DERIVED_STRUCT_SYMTAB, "derived", {},
                           parent=symbol_ref(MODULE_SYMTAB, "base"))},
        {}, [],
    ), "schema-invalid extension of a sequence type"


def invalid_array_constructor_element(_rng):
    element = f"(Array :type {integer_type(4)} " \
              f":dims [(dimension :start {integer_constant(1, 4)} " \
              f":length {integer_constant(2, 4)})] " \
              f":physical_type :FixedSizeArray)"
    constructor = (
        f"(ArrayConstructor :args [{integer_constant(1, 4)} "
        f"{integer_constant(2, 8)}] :type {element} :value nil "
        f":storage_format :ColMajor :struct_var nil)")
    return translation_unit(
        {"a": variable(PROGRAM_SYMTAB, "a", element)},
        [f"(Assignment :target {var(PROGRAM_SYMTAB, 'a')} "
         f":value {constructor} :overloaded nil :realloc_lhs false "
         f":move_allocation false)"],
    ), "schema-invalid array constructor element type"


TYPE_REACH_BUILDERS = [
    invalid_select_type_unrelated_guard,
    invalid_allocate_unrelated_type,
    invalid_sequence_type_extended,
    invalid_array_constructor_element,
]


def generate(mode, seed):
    if mode == "schema-valid":
        return generate_valid(seed)
    if mode == "schema-invalid":
        return generate_invalid(seed)
    raise ValueError(f"unknown schema generator mode {mode!r}")
