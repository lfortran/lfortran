from collections import defaultdict, deque
from itertools import chain
from pathlib import Path
from typing import (Any, Callable, Deque, Dict, Iterator, List, Optional, Set,
                    Tuple)

from llanguage_server.auxiliary_schema import AUXILIARY_SCHEMA
from llanguage_server.cxx.file_generator import CPlusPlusLspFileGenerator
from llanguage_server.utils import (any_enum, lower_first,
                                    method_to_camel_case, rename_enum,
                                    rename_type, upper_first)


class CPlusPlusLspTransformerSourceGenerator(CPlusPlusLspFileGenerator):
    generated_to_any: Set[str]

    def __init__(
        self,
        output_dir: Path,
        schema: Dict[str, Any],
        namespace: str,
        symbols: Dict[str, Tuple[str, Dict[str, Any]]]
    ) -> None:
        specification_source = output_dir / "lsp_transformer.cpp"
        super().__init__(specification_source, schema, namespace, symbols)
        self.generated_to_any = set()

    def generate_enumeration_transforms(self) -> None:
        enum_specs = chain(
            AUXILIARY_SCHEMA["enumerations"],
            self.schema["enumerations"],
        )
        for enum_spec in enum_specs:
            enum_name = enum_spec["name"]
            inst_name = lower_first(enum_name)
            type_spec = enum_spec["type"]
            type_name = type_spec["name"]
            enumerator = rename_enum(type_name)
            value_type = rename_type(type_name)
            self.write(f'auto LspTransformer::anyTo{upper_first(enum_name)}(')
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
        self,
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

    def get_type_name(self, type_spec: Dict[str, Any]) -> str:
        match type_spec["kind"]:
            case "base" | "reference":
                return type_spec["name"]
            case "literal":
                return self.nested_name()
            case _:
                raise ValueError(f'Cannot determine type name for: {type_spec}')

    def generate_upper_type_name(self, type_spec: Dict[str, Any]) -> None:
        type_name = self.get_type_name(type_spec)
        self.inline(upper_first(type_name))

    def generate_lower_type_name(self, type_spec: Dict[str, Any]) -> None:
        type_name = self.get_type_name(type_spec)
        self.inline(lower_first(type_name))

    def index_by_type(
        self,
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
        self,
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
        self,
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
                    self.write(f'value = anyTo{upper_first(enum_name)}(any);')
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
        self,
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
                    self.write(f'value = anyTo{upper_first(enum_name)}(any);')
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
        self,
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
                    self.write(f'value = anyTo{upper_first(enum_name)}(any);')
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
        self,
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
        self,
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

    def generate_any_to_nested_variant(self, spec: Dict[str, Any]) -> None:
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

        self.write(f'auto LspTransformer::anyTo{upper_first(nested_name)}(')
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

    def generate_nested_variant_to_any(self, spec: Dict[str, Any]) -> None:
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
        self,
        nested_names: Deque[str],
        spec: Dict[str, Any]
    ) -> None:
        with self.nested_names_as(nested_names) as nested_name:
            self.generate_any_to_nested_variant(spec)
            self.generate_nested_variant_to_any(spec)

    def generate_nested_any_to_structure(
        self,
        spec: Dict[str, Any]
    ) -> None:
        type_name = self.nested_name()
        self.write(f'auto LspTransformer::anyTo{upper_first(type_name)}(')
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
                                    self.inline(upper_first(ref_type_name))
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
        self,
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
        self,
        nested_names: Deque[str],
        spec: Dict[str, Any]
    ) -> None:
        with self.nested_names_as(nested_names) as nested_name:
            self.generate_nested_any_to_structure(spec)
            self.generate_nested_structure_to_any(spec)

    def generate_structure_transforms(self) -> None:
        self.write('// =================================== //')
        self.write('// LSPAny <-> LSP Structure Transforms //')
        self.write('// =================================== //')
        self.newline()
        struct_specs = chain(
            AUXILIARY_SCHEMA["structures"],
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
        self,
        alias_name: str,
        alias_spec: Dict[str, Any]
    ) -> None:
        self.write(f'auto LspTransformer::anyTo{upper_first(alias_name)}(')
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
        self,
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

    def generate_type_alias_transforms(self) -> None:
        self.write('// ==================================== //')
        self.write('// LSPAny <-> LSP Type Alias Transforms //')
        self.write('// ==================================== //')
        self.newline()
        alias_specs = chain(
            AUXILIARY_SCHEMA["typeAliases"],
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

    def generate_copy_any(self) -> None:
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

    def generate_copy_object(self) -> None:
        self.write('auto LspTransformer::copy(const LSPObject &object) const -> LSPObject {')
        with self.indent():
            self.write('LSPObject clone;')
            self.write('for (const auto &[key, value] : object) {')
            with self.indent():
                self.write('clone.emplace(key, copy(value));')
            self.write('}')
            self.write('return clone;')
        self.write('}')

    def generate_copy_array(self) -> None:
        self.write('auto LspTransformer::copy(const LSPArray &array) const -> LSPArray {')
        with self.indent():
            self.write('LSPArray clone;')
            self.write('for (const std::unique_ptr<LSPAny> &elem : array) {')
            with self.indent():
                self.write('clone.push_back(copy(elem));')
            self.write('}')
            self.write('return clone;')
        self.write('}')

    def generate_copy_methods(self) -> None:
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

    def generate_as_message_params_type(self, type_spec: Dict[str, Any]) -> None:
        match type_spec["kind"]:
            case "reference":
                self.inline(f'std::unique_ptr<{type_spec["name"]}>')
            case _:
                raise ValueError(f'Unsupported request parameter type: {type_spec}')

    def generate_as_incoming_params(
        self,
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
                                        self.inline(upper_first(or_type_name))
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

    def generate_result_to_any(self, request_spec: Dict[str, Any]) -> None:
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

    def generate_incoming_request_methods(self) -> None:
        self.write('// ================= //')
        self.write('// Incoming Requests //')
        self.write('// ================= //')
        self.newline()
        for request_spec in self.schema["requests"]:
            if request_spec["messageDirection"] == "clientToServer":
                self.generate_as_incoming_params("requestParams", request_spec)
                self.generate_result_to_any(request_spec)

    def generate_incoming_notification(self) -> None:
        self.write('// ====================== //')
        self.write('// Incoming Notifications //')
        self.write('// ====================== //')
        self.newline()
        for notification_spec in self.schema["notifications"]:
            if notification_spec["messageDirection"] == "clientToServer":
                self.generate_as_incoming_params("notificationParams", notification_spec)

    def generate_as_outgoing_params(
        self,
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
                                            #     pass
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

    def generate_outgoing_request_methods(self) -> None:
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

    def generate_outgoing_notification(self) -> None:
        self.write('// ====================== //')
        self.write('// Outgoing Notifications //')
        self.write('// ====================== //')
        self.newline()
        for notification_spec in self.schema["notifications"]:
            if notification_spec["messageDirection"] == "serverToClient":
                self.generate_as_outgoing_params("notificationParams", notification_spec)

    def generate_constructor(self) -> None:
        self.write('LspTransformer::LspTransformer(lsl::Logger &logger)')
        with self.indent():
            self.write(': logger(logger)')
        self.write('{')
        with self.indent():
            self.write('// empty')
        self.write('}')
        self.newline()

    def generate_code(self) -> None:
        print(f'Generating: {self.file_path}')
        self.generate_disclaimer()
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
