from functools import wraps
from pathlib import Path
from typing import Callable, List, Optional, Tuple

from llanguage_server.cxx.visitors import BaseCPlusPlusLspVisitor
from llanguage_server.lsp.datatypes import LspSpec, LspSymbol, LspSymbolKind
from llanguage_server.lsp.utils import (lower_first, rename_enum, rename_type,
                                        upper_first)
from llanguage_server.lsp.visitors import LspAnalysisPipeline
from llanguage_server.cxx.lsp_file_generator import gensym_context


def union_getter_fn(fn: Callable) -> Callable:
    @wraps(fn)
    def wrapper(self, *args, **kwargs):
        with self.type_declaration_options(
                wrap_ptr=False,
                wrap_nested_ptr=False,
                string_ptr=False,
                nested_string_ptr=False
        ):
            return fn(self, *args, **kwargs)
    return wrapper


FieldNamesAndParams = List[Tuple[str, List[str]]]


def noop(*args, **kwargs):
    pass


class CPlusPlusSpecificationSourceGenerator(BaseCPlusPlusLspVisitor):

    def __init__(
            self,
            output_dir: Path,
            schema: LspSpec,
            pipeline: LspAnalysisPipeline,
            namespace: str
    ) -> None:
        specification_source = output_dir / "lsp_specification.cpp"
        super().__init__(specification_source, schema, pipeline, namespace)

    def update_union_field(self, field_name: str, type_name: str, field_value: str) -> None:
        self.write(f'new (&{field_name}) std::unique_ptr<{type_name}> {{')
        with self.indent():
            self.write(field_value)
        self.write('};')

    def generate_enumeration(self, enum_spec: LspSpec) -> None:
        enum_name: str = enum_spec["name"]
        lower_enum = lower_first(enum_name)
        upper_enum = upper_first(enum_name)
        type_spec: LspSpec = enum_spec["type"]
        type_name: str = type_spec["name"]
        normalized_name = self.resolve(type_name).normalized_name
        is_string_type = (normalized_name == "string")
        value_type = self.rename_base_type(type_name)
        names_by_enum = f'{enum_name}Names'
        with self.gen_map(names_by_enum, enum_name, 'std::string'):
            for value_spec in enum_spec["values"]:
                value_name = rename_enum(value_spec["name"])
                map_key = f'{enum_name}::{value_name}'
                map_value = f'"{value_name}"'
                self.gen_map_entry(map_key, map_value)
        self.newline()
        if is_string_type:
            values_by_enum = f'{enum_name}Values'
            with self.gen_map(values_by_enum, enum_name, value_type):
                value_template = '"{enum_value}"'
                for value_spec in enum_spec["values"]:
                    value_name = rename_enum(value_spec["name"])
                    key = f'{enum_name}::{value_name}'
                    value = value_template.format(enum_value=value_spec["value"])
                    self.gen_map_entry(key, value)
            self.newline()
        else:
            values_by_enum = names_by_enum
        enum_by_name = f'{lower_enum}ByName'
        with self.gen_fn(enum_by_name, enum_name, params=['const std::string &name']):
            with self.gen_foreach('const auto &[enum_name, enum_value]', names_by_enum):
                with self.gen_if('name == enum_value'):
                    self.write('return enum_name;')
            self.gen_throw(
                'std::invalid_argument',
                f'"Invalid {enum_name} name: " + name'
            )
        self.newline()
        enum_by_value = f'{lower_enum}ByValue'
        if is_string_type:
            param = f'const {value_type} &value'
            if_expr = 'value == enum_value'
        else:
            param = f'{value_type} value'
            if_expr = f'value == static_cast<{value_type}>(enum_name)'
        with self.gen_fn(enum_by_value, enum_name, params=[param]):
            with self.gen_foreach('const auto &[enum_name, enum_value]', values_by_enum):
                with self.gen_if(if_expr):
                    self.write('return enum_name;')
            self.gen_throw(
                'std::invalid_argument',
                f'"Invalid {enum_name} value: \\"" + value + "\\""'
                if is_string_type else
                f'"Invalid {enum_name} value: " + std::to_string(value)'
            )
        self.newline()
        if is_string_type:
            is_enum = f'is{upper_enum}'
            with self.gen_fn(is_enum, 'bool', params=[param]):
                with self.gen_foreach('const auto &[enum_key, enum_value]', values_by_enum):
                    with self.gen_if(if_expr):
                        self.write('return true;')
                self.write('return false;')
        self.newline()

    def gen_copy_array_3(
            self,
            array_symbol: LspSymbol,
            this_array: str,
            that_array: str,
            this_is_pointer: bool = False,
            that_is_pointer: bool = False,
            init_array: Callable[[], None] = noop
    ) -> None:
        this_accessor = that_accessor = '.'
        that_deref = ''
        if this_is_pointer:
            this_accessor = '->'
        if that_is_pointer:
            that_accessor = '->'
            that_deref = '*'
        array_spec = array_symbol.spec
        elem_spec: LspSpec = array_spec['element']
        init_array()
        self.gen_call(f'{this_array}{this_accessor}reserve', f'{that_array}{that_accessor}size()')
        with self.gensym_foreach(f'{that_deref}{that_array}') as elem_nym:
            if self.has_cycle(array_symbol, elem_spec):
                with self.nest_name('elem'):
                    with self.type_declaration_options(wrap_ptr=False):
                        elem_type = self.get_type_declaration(elem_spec)
                    elem_expr = f'std::make_unique<{elem_type}>(*{elem_nym})'
            else:
                elem_expr = elem_nym
            self.gen_call(f'{this_array}{this_accessor}push_back', elem_expr)

    def gen_copy_array_2(
            self,
            array_symbol: LspSymbol,
            this_array: str,
            that_array: str,
            this_is_pointer: bool = False,
            that_is_pointer: bool = False,
            init_array: Callable[[], None] = noop
    ) -> None:
        self.gen_copy_array_3(
            array_symbol,
            this_array,
            that_array,
            this_is_pointer,
            that_is_pointer,
            init_array
        )

    def gen_copy_array_1(
            self,
            array_symbol: LspSymbol,
            this_array: str,
            that_array: str,
            this_is_pointer: bool = False,
            that_is_pointer: bool = False,
            init_array: Callable[[], None] = noop
    ) -> None:
        array_spec = array_symbol.spec
        that_expr = that_array
        if that_is_pointer:
            that_expr = f'*{that_expr}'
        this_expr = that_expr
        if this_is_pointer:
            with self.type_declaration_options(wrap_ptr=False):
                type_name = self.get_type_declaration(array_spec)
            this_expr = f'std::make_unique<{type_name}>({that_expr})'
        elem_spec: LspSpec = array_spec["element"]
        match elem_spec["kind"]:
            case "base":
                self.gen_assign(this_array, this_expr)
            case "reference":
                elem_type_name = elem_spec["name"]
                match elem_type_name:
                    case "LSPAny":
                        self.gen_copy_array_3(
                            array_symbol,
                            this_array,
                            that_array,
                            this_is_pointer,
                            that_is_pointer,
                            init_array
                        )
                    case _:
                        elem_symbol = self.resolve(elem_type_name)
                        match elem_symbol.kind:
                            case LspSymbolKind.STRUCTURE:
                                self.gen_copy_array_2(
                                    array_symbol,
                                    this_array,
                                    that_array,
                                    this_is_pointer,
                                    that_is_pointer,
                                    init_array
                                )
                            case LspSymbolKind.ENUMERATION | LspSymbolKind.UNION:
                                self.gen_assign(this_array, this_expr)
                            case _:
                                raise ValueError(f'Unsupported array type ({elem_symbol.kind}): {elem_symbol.spec}')
            case "or":
                self.gen_assign(this_array, this_expr)
            case "literal":
                self.gen_copy_array_2(
                    array_symbol,
                    this_array,
                    that_array,
                    this_is_pointer,
                    that_is_pointer,
                    init_array
                )
            case _:
                raise ValueError(
                    f'Unsupported array type ({elem_spec["kind"]}): {elem_spec}'
                )

    def gen_struct_copy_array_1(
            self,
            field_name: str,
            array_symbol: LspSymbol,
            is_optional: bool
    ) -> None:
        if is_optional:
            with self.gen_if(f'other.{field_name}.has_value()'):
                this_array = self.gensym_ref('array', f'{field_name}.emplace()', const=False)
                that_array = self.gensym_ref("array", f'other.{field_name}.value()')
                self.gen_copy_array_1(array_symbol, this_array, that_array)
        else:
            self.gen_copy_array_1(array_symbol, field_name, f'other.{field_name}')

    def gen_copy_map_1(
            self,
            map_symbol: LspSymbol,
            this_map: str,
            that_map: str,
            this_is_pointer: bool = False,
            that_is_pointer: bool = False,
            init_this: Callable[[], None] = noop
    ) -> None:
        map_spec = map_symbol.spec

        this_accessor = '.'
        that_deref = ''
        if this_is_pointer:
            this_accessor = '->'
        if that_is_pointer:
            that_deref = '*'

        that_expr = f'{that_deref}{that_map}'
        this_expr = that_expr
        if this_is_pointer:
            with self.type_declaration_options(wrap_ptr=False):
                map_type = self.get_type_declaration(map_symbol)
                this_expr = f'std::make_unique<{map_type}>({this_expr})'

        with self.nest_name('key'):
            key_spec: LspSpec = map_spec["key"]
            match key_spec["kind"]:
                case "base":
                    key_type_name: str = key_spec["name"]
                case "reference":
                    key_symbol = self.resolve(key_spec["name"])
                    match key_symbol.kind:
                        case LspSymbolKind.BASE:
                            key_type_name = key_symbol.name
                        case _:
                            raise ValueError(f'Unsupported map-key type ({key_symbol.kind}): {key_spec}')
                case _:
                    raise ValueError(f'Unsupported may-key type ({key_spec["kind"]}): {key_spec}')

            if self.resolve(key_type_name).normalized_name != "string":
                raise ValueError(f'Unsupported map-key type ({key_type_name}): {key_spec}')

        with self.nest_name('value') as value_name:
            value_spec = map_spec["value"]
            match value_spec["kind"]:
                case "base":
                    self.gen_assign(this_map, this_expr)
                case "reference":
                    value_type_name = value_spec["name"]
                    value_symbol = self.resolve(value_spec["name"])
                    match value_symbol.kind:
                        case LspSymbolKind.STRUCTURE | LspSymbolKind.UNION:
                            init_this()
                            with self.gensym_foreach_keyval(that_expr) as (key_nym, value_nym):
                                if self.has_cycle(map_symbol, value_spec):
                                    value_expr = f'std::make_unique<{value_type_name}>(*{value_nym})'
                                else:
                                    value_expr = value_nym
                                self.gen_call(f'{this_map}{this_accessor}emplace', key_nym, value_expr)
                        case _:
                            raise ValueError(
                                f'Unsupported map-value type ({value_symbol.kind}): {value_symbol.spec}'
                            )
                case "or":
                    self.gen_assign(this_map, this_expr)
                case "array":
                    array_spec: LspSpec = value_spec
                    elem_spec: LspSpec = array_spec["element"]
                    match elem_spec["kind"]:
                        case "reference":
                            value_type_name = elem_spec["name"]
                            value_symbol = self.resolve(value_type_name)
                            match value_symbol.kind:
                                case LspSymbolKind.STRUCTURE:
                                    array_symbol = self.resolve(value_name)
                                    array_type = self.get_type_declaration(array_spec)
                                    with self.gensym_foreach_keyval(that_expr) as (key_nym, value_nym):
                                        this_array = self.gensym_decl(array_type, 'array')
                                        that_array = value_nym
                                        self.gen_copy_array_1(
                                            array_symbol,
                                            this_array,
                                            that_array,
                                            # this_is_pointer=self._type_declaration_wrap_ptr,
                                            # that_is_pointer=self._type_declaration_wrap_ptr
                                            this_is_pointer=False,
                                            that_is_pointer=False
                                        )
                                        self.gen_call(
                                            f'{this_map}{this_accessor}emplace',
                                            key_nym,
                                            f'std::move({this_array})'
                                        )
                                case _:
                                    raise ValueError(
                                        f'Unsupported array type ({value_symbol.kind}): {value_symbol.spec}'
                                    )
                        case _:
                            raise ValueError(f'Unsupported array type ({elem_spec["kind"]}): {elem_spec}')
                case _:
                    raise ValueError(f'Unsupported map-value type ({value_spec["kind"]}): {value_spec}')

    def gen_struct_copy_map_1(
            self,
            field_name: str,
            map_symbol: LspSymbol,
            is_optional: bool
    ) -> None:
        if is_optional:
            with self.gen_if(f'other.{field_name}.has_value()'):
                this_map = self.gensym_ref("map", f'{field_name}.emplace()', const=False)
                that_map = self.gensym_ref("map", f'other.{field_name}.value()')
                self.gen_copy_map_1(map_symbol, this_map, that_map)
        else:
            self.gen_copy_map_1(map_symbol, field_name, f'other.{field_name}')

    def gen_copy_tuple_1(
            self,
            tuple_spec: LspSpec,
            this_tuple: str,
            that_tuple: str,
            this_is_pointer: bool = False,
            that_is_pointer: bool = False
    ) -> None:
        item_specs = tuple_spec["items"]
        for item_spec in item_specs:
            if item_spec["kind"] != "base" \
               or self.resolve(item_spec["name"]).normalized_name == "string":
                raise ValueError(f'Unsupported tuple type: {item_spec}')
        that_expr = that_tuple
        if that_is_pointer:
            that_expr = f'*{that_expr}'
        this_expr = that_expr
        if this_is_pointer:
            with self.type_declaration_options(wrap_ptr=False):
                tuple_type = self.get_type_declaration(tuple_spec)
                this_expr = f'std::make_unique<{tuple_type}>({this_expr})'
        self.gen_assign(this_tuple, this_expr)

    def gen_struct_copy_tuple_1(
            self,
            field_name: str,
            tuple_spec: LspSpec,
            is_optional: bool
    ) -> None:
        if is_optional:
            raise NotImplementedError(
                f'Optional tuples are not supported: {tuple_spec}'
            )
        else:
            self.gen_copy_tuple_1(tuple_spec, field_name, f'other.{field_name}')

    def generate_struct_copy_body_1(
            self,
            field_names_and_params: Optional[FieldNamesAndParams],
            field_name: str,
            is_optional: bool,
            gen_optional: bool,
            gen_required: bool,
            collect_required: bool
    ) -> None:
        if is_optional:
            if gen_optional:
                with self.gen_if(f'other.{field_name}.has_value()'):
                    self.gen_assign(field_name, f'other.{field_name}.value()')
        else:
            if gen_required:
                self.gen_assign(field_name, f'other.{field_name}')
            if collect_required and field_names_and_params is not None:
                field_names_and_params.append((field_name, [
                    f'other.{field_name}'
                ]))

    def generate_struct_copy_body_reference_other_struct(
            self,
            struct_symbol: LspSymbol,
            field_names_and_params: Optional[FieldNamesAndParams],
            field_name: str,
            field_type_name: str,
            is_optional: bool,
            gen_optional: bool,
            gen_required: bool,
            collect_required: bool
    ) -> None:
        if is_optional:
            if gen_optional:
                with self.gen_if(f'other.{field_name}.has_value()'):
                    dependency = self.resolve(field_type_name)
                    that_expr = f'other.{field_name}.value()'
                    this_expr = that_expr
                    if self.has_cycle(struct_symbol, dependency):
                        this_expr = f'std::make_unique<{field_type_name}>(*{this_expr})'
                    self.gen_assign(field_name, this_expr)
        else:
            if gen_required:
                self.gen_assign(field_name, f'other.{field_name}')
            if collect_required and field_names_and_params is not None:
                field_names_and_params.append((field_name, [
                    f'other.{field_name}'
                ]))

    def generate_struct_copy_body_reference_other_base_or_enum(
            self,
            field_names_and_params: Optional[FieldNamesAndParams],
            field_name: str,
            is_optional: bool,
            gen_optional: bool,
            gen_required: bool,
            collect_required: bool
    ) -> None:
        if is_optional:
            if gen_optional:
                with self.gen_if(f'other.{field_name}.has_value()'):
                    self.gen_assign(field_name, f'other.{field_name}.value()')
        else:
            if gen_required:
                self.gen_assign(field_name, f'other.{field_name}')
            if collect_required and field_names_and_params is not None:
                field_names_and_params.append((field_name, [f'other.{field_name}']))

    def generate_struct_copy_body_reference_other_union(
            self,
            field_names_and_params: Optional[FieldNamesAndParams],
            field_name: str,
            field_symbol: LspSymbol,
            is_optional: bool,
            gen_optional: bool,
            gen_required: bool,
            collect_required: bool
    ) -> None:
        if is_optional:
            if gen_optional:
                with self.gen_if(f'other.{field_name}.has_value()'):
                    self.gen_assign(field_name, f'other.{field_name}.value()')
        else:
            if gen_required:
                self.gen_assign(field_name, f'{field_symbol.name}(other.{field_name})')
            if collect_required and field_names_and_params is not None:
                field_names_and_params.append((field_name, [
                    f'{field_symbol.name}(other.{field_name})'
                ]))

    def generate_struct_copy_body_reference_other(
            self,
            struct_symbol: LspSymbol,
            field_names_and_params: Optional[FieldNamesAndParams],
            field_name: str,
            field_type_name: str,
            is_optional: bool,
            gen_optional: bool,
            gen_required: bool,
            gen_collections: bool,
            collect_required: bool
    ) -> None:
        field_symbol = self.resolve(field_type_name)
        match field_symbol.kind:
            case LspSymbolKind.STRUCTURE:
                self.generate_struct_copy_body_reference_other_struct(
                    struct_symbol,
                    field_names_and_params,
                    field_name,
                    field_type_name,
                    is_optional,
                    gen_optional,
                    gen_required,
                    collect_required
                )
            case LspSymbolKind.BASE | LspSymbolKind.ENUMERATION:
                self.generate_struct_copy_body_reference_other_base_or_enum(
                    field_names_and_params,
                    field_name,
                    is_optional,
                    gen_optional,
                    gen_required,
                    collect_required,
                )
            case LspSymbolKind.UNION:
                self.generate_struct_copy_body_reference_other_union(
                    field_names_and_params,
                    field_name,
                    field_symbol,
                    is_optional,
                    gen_optional,
                    gen_required,
                    collect_required,
                )
            case LspSymbolKind.ARRAY:
                if gen_collections:
                    self.gen_struct_copy_array_1(field_name, field_symbol, is_optional)
            case LspSymbolKind.MAP:
                if gen_collections:
                    self.gen_struct_copy_map_1(field_name, field_symbol, is_optional)
            case LspSymbolKind.TUPLE:
                if gen_collections:
                    self.gen_struct_copy_tuple_1(field_name, field_symbol.spec, is_optional)

    def generate_struct_copy_body_reference(
            self,
            struct_symbol: LspSymbol,
            field_names_and_params: Optional[FieldNamesAndParams],
            field_name: str,
            field_type: LspSpec,
            is_optional: bool,
            gen_optional: bool,
            gen_required: bool,
            gen_collections: bool,
            collect_required: bool
    ) -> None:
        field_type_name = field_type["name"]
        match field_type_name:
            # case "LSPAny":
            #     pass
            case "LSPObject":
                if not collect_required:
                    object_symbol = self.resolve(field_type_name)
                    self.gen_struct_copy_map_1(field_name, object_symbol, is_optional)
            case "LSPArray":
                if not collect_required:
                    array_symbol = self.resolve(field_type_name)
                    self.gen_struct_copy_array_1(field_name, array_symbol, is_optional)
            case _:
                self.generate_struct_copy_body_reference_other(
                    struct_symbol,
                    field_names_and_params,
                    field_name,
                    field_type_name,
                    is_optional,
                    gen_optional,
                    gen_required,
                    gen_collections,
                    collect_required
                )

    def generate_struct_copy_body_literal(
            self,
            field_names_and_params: Optional[FieldNamesAndParams],
            struct_name: str,
            field_name: str,
            is_optional: bool,
            gen_optional: bool,
            gen_required: bool,
            collect_required: bool
    ) -> None:
        type_name = self.nested_name([struct_name, field_name])
        if is_optional:
            if gen_optional:
                with self.gen_if(f'other.{field_name}.has_value()'):
                    # self.gen_assign(
                    #     field_name,
                    #     f'std::make_unique<{type_name}>(other.{field_name}.value())'
                    # )
                    self.gen_assign(field_name, f'other.{field_name}.value()')
        else:
            if gen_required:
                self.gen_assign(
                    field_name,
                    f'other.{field_name}'
                )
            if collect_required and field_names_and_params is not None:
                field_names_and_params.append((field_name, [
                    f'other.{field_name}'
                ]))

    def generate_struct_copy_body_union(
            self,
            field_names_and_params: Optional[FieldNamesAndParams],
            struct_name: str,
            field_name: str,
            is_optional: bool,
            gen_optional: bool,
            gen_required: bool,
            collect_required: bool
    ) -> None:
        type_name = self.nested_name([struct_name, field_name])
        if is_optional:
            if gen_optional:
                with self.gen_if(f'other.{field_name}.has_value()'):
                    # self.gen_assign(
                    #     field_name,
                    #     f'std::make_unique<{type_name}>(other.{field_name}.value())'
                    # )
                    self.gen_assign(field_name, f'other.{field_name}.value()')
        else:
            if gen_required:
                self.gen_assign(field_name, f'{type_name}(other.{field_name})')
            if collect_required and field_names_and_params is not None:
                field_names_and_params.append((field_name, [f'{type_name}(other.{field_name})']))

    def generate_struct_copy_body(
            self,
            struct_symbol: LspSymbol,
            gen_optional: bool = True,
            gen_required: bool = True,
            gen_collections: bool = True
    ) -> Optional[FieldNamesAndParams]:
        collect_required: bool = not (gen_optional or gen_required or gen_collections)
        struct_name = struct_symbol.name

        field_names_and_params = None
        if collect_required:
            field_names_and_params = []

        if struct_symbol.fields is not None:
            for field_spec in struct_symbol.fields:
                field_name = field_spec["name"]
                with self.nest_name(field_name):
                    field_type = field_spec["type"]
                    is_optional = field_spec.get("optional", False)
                    match field_type["kind"]:
                        case "stringLiteral" | "integerLiteral" | "booleanLiteral":
                            # These are constants, there is nothing to copy ...
                            pass
                        case "base":
                            self.generate_struct_copy_body_1(
                                field_names_and_params,
                                field_name,
                                is_optional,
                                gen_optional,
                                gen_required,
                                collect_required
                            )
                        case "reference":
                            self.generate_struct_copy_body_reference(
                                struct_symbol,
                                field_names_and_params,
                                field_name,
                                field_type,
                                is_optional,
                                gen_optional,
                                gen_required,
                                gen_collections,
                                collect_required
                            )
                        case "literal":
                            self.generate_struct_copy_body_literal(
                                field_names_and_params,
                                struct_name,
                                field_name,
                                is_optional,
                                gen_optional,
                                gen_required,
                                collect_required,
                            )
                        case "or":
                            self.generate_struct_copy_body_union(
                                field_names_and_params,
                                struct_name,
                                field_name,
                                is_optional,
                                gen_optional,
                                gen_required,
                                collect_required,
                            )
                        case "array":
                            if gen_collections:
                                array_name = self.nested_name()
                                array_symbol = self.resolve(array_name)
                                self.gen_struct_copy_array_1(field_name, array_symbol, is_optional)
                        case "map":
                            if gen_collections:
                                map_name = self.nested_name()
                                map_symbol = self.resolve(map_name)
                                self.gen_struct_copy_map_1(field_name, map_symbol, is_optional)
                        case "tuple":
                            if gen_collections:
                                self.gen_struct_copy_tuple_1(field_name, field_type, is_optional)
                        case _:
                            raise ValueError(f'Unsupported field type ({field_type["kind"]}): {field_spec}')

        return field_names_and_params

    def generate_struct_move_body(self, struct_symbol: LspSymbol) -> None:
        if struct_symbol.fields is not None:
            for field_spec in struct_symbol.fields:
                field_name = field_spec["name"]
                with self.nest_name(field_name):
                    field_type = field_spec["type"]
                    match field_type["kind"]:
                        case "base":
                            if self.resolve(field_type["name"]).normalized_name == "string" \
                               or field_spec.get('optional', False):
                                self.gen_assign(field_name, f'std::move(other.{field_name})')
                            else:
                                self.gen_assign(field_name, f'other.{field_name}')
                        case "stringLiteral" | "reference" | "or" | "array" | "map" | "literal":
                            self.gen_assign(field_name, f'std::move(other.{field_name})')
                        case _:
                            raise ValueError(f'Unsupported field type ({field_type["kind"]}): {field_spec}')

    def gen_struct_move_constructor(
            self,
            struct_symbol: LspSymbol,
            specs: Optional[str] = None
    ) -> None:
        struct_name = struct_symbol.name
        struct_spec = struct_symbol.spec
        field_specs = struct_symbol.fields
        extends_specs = struct_spec.get("extends", None)
        uses_param = (((field_specs is not None)
                       and (len(field_specs) > 0))
                      or (extends_specs is not None))

        inits = []
        if field_specs is not None:
            for field_spec in field_specs:
                field_name = field_spec["name"]
                field_type = field_spec["type"]
                match field_type["kind"]:
                    case "base":
                        if self.resolve(field_type["name"]).normalized_name == "string" \
                           or field_spec.get('optional', False):
                            inits.append((field_name, [f'std::move(other.{field_name})']))
                        else:
                            inits.append((field_name, [f'other.{field_name}']))
                    case "stringLiteral" | "reference" | "or" | "array" | "map" | "literal":
                        inits.append((field_name, [f'std::move(other.{field_name})']))
                    case _:
                        raise ValueError(
                            f'Unsupported field type ({field_type["kind"]}): {field_spec}'
                        )

        if len(inits) == 0:
            inits = None

        sup_inits = None
        if extends_specs is not None:
            sup_inits = []
            for extends_spec in extends_specs:
                sup_inits.append((extends_spec, ["other"]))

        if uses_param:
            param = f'{struct_name} &&other'
        else:
            param = f'{struct_name} &&/*other*/'

        with self.gen_constructor(
                struct_name,
                params=[param],
                inits=inits,
                specs=specs,
                sups=sup_inits
        ):
            self.write('// empty')
        self.newline()

    @gensym_context
    def gen_struct_copy_constructor(
            self,
            struct_symbol: LspSymbol,
            specs: Optional[str] = None
    ) -> None:
        struct_name = struct_symbol.name
        struct_spec = struct_symbol.spec
        field_specs = struct_symbol.fields
        extends_specs = struct_spec.get("extends", None)
        uses_param = (((field_specs is not None)
                       and (len(field_specs) > 0))
                      or (extends_specs is not None))

        inits = self.generate_struct_copy_body(
            struct_symbol,
            gen_optional=False,
            gen_required=False,
            gen_collections=False
        )

        if inits is not None and len(inits) == 0:
            inits = None

        sup_inits = None
        if extends_specs is not None:
            sup_inits = []
            for extends_spec in extends_specs:
                sup_inits.append((extends_spec, ["other"]))

        if uses_param:
            param = f'const {struct_name} &other'
        else:
            param = f'const {struct_name} &/*other*/'

        with self.gen_constructor(
                struct_name,
                params=[param],
                inits=inits,
                specs=specs,
                sups=sup_inits
        ):
            self.generate_struct_copy_body(
                struct_symbol,
                gen_required=False
            )
        self.newline()

    @gensym_context
    def gen_struct_move_assign(self, struct_symbol: LspSymbol) -> None:
        struct_name: str = struct_symbol.name
        field_specs = struct_symbol.fields
        struct_spec = struct_symbol.spec
        extends_specs = struct_spec.get("extends", None)
        uses_param = (((field_specs is not None)
                       and (len(field_specs) > 0))
                      or (extends_specs is not None))
        if uses_param:
            param = f'{struct_name} &&other'
        else:
            param = f'{struct_name} &&/*other*/'
        with self.gen_fn(
                f'{struct_name}::operator=',
                f'{struct_name} &',
                params=[param],
        ):
            if uses_param:
                with self.gen_if('this != &other'):
                    if extends_specs is not None:
                        for extends_spec in extends_specs:
                            super_name = extends_spec["name"]
                            self.gen_call(f'{super_name}::operator=', 'other')
                    self.generate_struct_move_body(struct_symbol)
            self.write('return *this;')
        self.newline()

    @gensym_context
    def gen_struct_copy_assign(self, struct_symbol: LspSymbol) -> None:
        struct_name: str = struct_symbol.name
        field_specs = struct_symbol.fields
        struct_spec = struct_symbol.spec
        extends_specs = struct_spec.get("extends", None)
        uses_param = (((field_specs is not None)
                       and (len(field_specs) > 0))
                      or (extends_specs is not None))
        if uses_param:
            param = f'const {struct_name} &other'
        else:
            param = f'const {struct_name} &/*other*/'
        with self.gen_fn(
                f'{struct_name}::operator=',
                f'{struct_name} &',
                params=[param],
        ):
            if uses_param:
                with self.gen_if('this != &other'):
                    if extends_specs is not None:
                        for extends_spec in extends_specs:
                            super_name = extends_spec["name"]
                            self.gen_call(f'{super_name}::operator=', 'other')
                    self.generate_struct_copy_body(struct_symbol)
            self.write('return *this;')
        self.newline()

    def gen_struct_constructor(self, struct_symbol: LspSymbol) -> None:
        struct_spec = struct_symbol.spec
        struct_name = struct_symbol.name
        extends_specs = struct_spec.get("extends", None)
        with self.gen_constructor(struct_name, sups=extends_specs):
            self.write('// empty')
        self.newline()

    def gen_struct_destructor(self, struct_symbol: LspSymbol) -> None:
        struct_name = struct_symbol.name
        with self.gen_destructor(struct_name):
            self.write('// empty')
        self.newline()

    def generate_structure(self, struct_symbol: LspSymbol) -> None:
        self.gen_struct_constructor(struct_symbol)
        self.gen_struct_move_constructor(struct_symbol)
        self.gen_struct_copy_constructor(struct_symbol)
        self.gen_struct_destructor(struct_symbol)
        self.gen_struct_move_assign(struct_symbol)
        self.gen_struct_copy_assign(struct_symbol)

    @union_getter_fn
    def generate_union_field_getter(
            self,
            union_name: str,
            enum_name: str,
            field_name: str,
            field_spec: LspSpec) -> None:
        field_type = self.get_type_declaration(field_spec)
        field_expr = f'_{field_name}'
        if self.is_union_pointer_type(field_spec):
            field_expr = f'*{field_expr}'
        enum_value = self.get_enum_value(enum_name, field_spec)
        names_by_enum = f'{enum_name}Names'
        with self.gen_fn(
                f'{union_name}::{field_name}',
                f'const {field_type} &',
                specs='const'
        ):
            with self.gen_if_debug_enabled(f'_type != {enum_value}'):
                self.write('throw std::logic_error(')
                with self.indent():
                    self.write(f'("Attempted to access "')
                    self.write(f' "{union_name}::{field_name}()"')
                    self.write(f' " of type "')
                    self.write(f' "{enum_value}"')
                    self.write(f' " while active type is "')
                    self.write(f' "{enum_name}::" +')
                    self.write(f' {names_by_enum}.at(_type))')
                self.write(');')
            self.write(f'return {field_expr};')
        self.newline()

    @union_getter_fn
    def generate_union_tag_getter(self, union_name: str) -> None:
        field_name = 'type'
        enum_name = f'{union_name}Type'
        with self.gen_fn(
                f'{union_name}::{field_name}',
                f'const {enum_name} &',
                specs='const'
        ):
            self.write(f'return _{field_name};')
        self.newline()

    def generate_union_move_body(
        self,
        enum_name: str,
        field_specs: List[LspSpec]
    ) -> None:
        with self.gen_switch('other._type'):
            for field_index, field_spec in enumerate(field_specs):
                with self.nest_name(str(field_index)):
                    field_name = self.name_field(field_spec)
                    field_name = f'_{field_name}'
                    with self.gen_case(enum_name, field_spec):
                        match field_spec["kind"]:
                            case "base" | "reference":
                                type_name = field_spec["name"]
                            case "literal" | "array" | "tuple" | "or" | "map":
                                type_name = self.nested_name()
                            case _:
                                raise ValueError(
                                    f'Unsupported union type ({field_spec["kind"]}): {field_spec}'
                                )
                        type_symbol = self.resolve(type_name)
                        type_name = rename_type(type_name)
                        is_pointer_type = True
                        match type_symbol.kind:
                            case LspSymbolKind.BASE:
                                is_pointer_type = (type_symbol.normalized_name == "string")
                            case LspSymbolKind.ENUMERATION:
                                is_pointer_type = False
                        if is_pointer_type:
                            self.update_union_field(
                                field_name, type_name,
                                f'std::move(other.{field_name})'
                            )
                        else:
                            self.gen_assign(field_name, f'other.{field_name}')
                        self.write('break;')
            with self.gen_case(enum_name, "Uninitialized"):
                self.write('// other has not been initialized, there is nothing to do ...')
                self.write('break;')
        self.gen_assign(
            f'other._type',
            f'{enum_name}::Uninitialized'
        )

    def generate_union_copy_field(
            self,
            union_symbol: LspSymbol,
            field_spec: LspSpec,
            qualifier: str = ''
    ) -> None:
        field_name = self.name_field(field_spec)
        type_name: str
        match field_spec["kind"]:
            case "base":
                type_name = field_spec["name"]
            case "reference":
                type_name = field_spec["name"]
            case "literal" | "or" | "array" | "map" | "tuple":
                type_name = self.nested_name()
            case _:
                raise ValueError(f'Unsupported union type ({field_spec["kind"]}): {field_spec}')
        with self.nested_name_as(type_name):
            symbol = self.resolve(type_name)
            type_name = rename_type(type_name)
            match symbol.kind:
                case LspSymbolKind.BASE:
                    if self.resolve(field_spec["name"]).normalized_name == "string":
                        prefix = '*' if self._type_declaration_string_ptr else ''
                        # self.gen_assign(
                        #     f'_{field_name}',
                        #     f'std::make_unique<{type_name}>({prefix}{qualifier}{field_name})'
                        # )
                        self.update_union_field(
                            f'_{field_name}', type_name,
                            f'std::make_unique<{type_name}>({prefix}{qualifier}{field_name})'
                        )
                    else:
                        self.gen_assign(f'_{field_name}', f'{qualifier}{field_name}')
                case LspSymbolKind.STRUCTURE | LspSymbolKind.INNER | LspSymbolKind.UNION:
                    prefix = '*' if self._type_declaration_wrap_ptr else ''
                    # self.gen_assign(
                    #     f'_{field_name}',
                    #     f'std::make_unique<{type_name}>({prefix}{qualifier}{field_name})'
                    # )
                    self.update_union_field(
                        f'_{field_name}', type_name,
                        f'std::make_unique<{type_name}>({prefix}{qualifier}{field_name})'
                    )
                case LspSymbolKind.ENUMERATION:
                    self.gen_assign(f'_{field_name}', f'{qualifier}{field_name}')
                case LspSymbolKind.ARRAY:
                    with self.type_declaration_options(wrap_ptr=False):
                        array_type = self.get_type_declaration(symbol.spec)
                    that_array = f'{qualifier}{field_name}'
                    self.gen_copy_array_1(
                        symbol,
                        f'_{field_name}',
                        that_array,
                        this_is_pointer=True,
                        that_is_pointer=self._type_declaration_wrap_ptr,
                        init_array=lambda: self.update_union_field(
                            f'_{field_name}', array_type,
                            f'std::make_unique<{array_type}>()'
                        )
                    )
                case LspSymbolKind.MAP:
                    with self.type_declaration_options(wrap_ptr=False):
                        map_type = self.get_type_declaration(symbol.spec)
                    that_map = f'{qualifier}{field_name}'
                    self.gen_copy_map_1(
                        symbol,
                        f'_{field_name}',
                        that_map,
                        this_is_pointer=True,
                        that_is_pointer=self._type_declaration_wrap_ptr,
                        init_this=lambda: self.update_union_field(
                            f'_{field_name}', map_type,
                            f'std::make_unique<{map_type}>()'
                        )
                    )
                case LspSymbolKind.TUPLE:
                    that_tuple = f'{qualifier}{field_name}'
                    self.gen_copy_tuple_1(
                        symbol.spec,
                        f'_{field_name}',
                        that_tuple,
                        this_is_pointer=True,
                        that_is_pointer=self._type_declaration_wrap_ptr
                    )
                case _:
                    raise ValueError(
                        f'Unsupported reference kind ({symbol.kind}): {symbol.spec}'
                    )

    def generate_union_copy_body(
            self,
            union_symbol: LspSymbol,
            enum_name: str,
            field_specs: List[LspSpec]
    ) -> None:
        with self.gen_switch('other._type'):
            for field_index, field_spec in enumerate(field_specs):
                with self.nest_name(str(field_index)):
                    with self.gen_case(enum_name, field_spec):
                        self.generate_union_copy_field(
                            union_symbol,
                            field_spec,
                            qualifier='other._'
                        )
                        self.gen_break()
            with self.gen_case(enum_name, "Uninitialized"):
                self.write('// other has not been initialized, there is nothing to do ...')
                self.gen_break()

    def gen_union_move_constructor(
            self,
            union_name: str,
            enum_name: str,
            item_specs: List[LspSpec]
    ) -> None:
        with self.gen_constructor(
                union_name,
                params=[
                    f'{union_name} &&other',
                ],
                inits=[
                    ('_type', ['other._type']),
                ],
        ):
            with self.type_declaration_options(
                    wrap_ptr=True,
                    wrap_nested_ptr=False,
                    string_ptr=True,
                    nested_string_ptr=False,
            ):
                self.generate_union_move_body(enum_name, item_specs)
        self.newline()

    @gensym_context
    def gen_union_copy_constructor(
            self,
            union_symbol: LspSymbol,
            union_name: str,
            enum_name: str,
            item_specs: List[LspSpec]
    ) -> None:
        with self.gen_constructor(
                union_name,
                params=[
                    f'const {union_name} &other',
                ],
                inits=[
                    ('_type', ['other._type']),
                ],
        ):
            with self.type_declaration_options(
                    wrap_ptr=True,
                    wrap_nested_ptr=False,
                    string_ptr=True,
                    nested_string_ptr=False,
            ):
                self.generate_union_copy_body(
                    union_symbol,
                    enum_name,
                    item_specs
                )
        self.newline()

    def gen_union_constructor(self, union_name: str) -> None:
        with self.gen_constructor(union_name):
            self.write('// empty')
        self.newline()

    def gen_union_destructor(self, union_name: str, has_pointer: bool) -> None:
        with self.gen_destructor(union_name):
            if has_pointer:
                self.gen_call('reset')
            else:
                self.write('// empty')
        self.newline()

    @gensym_context
    def gen_union_move_assign(
            self,
            union_name: str,
            enum_name: str,
            item_specs: List[LspSpec]
    ) -> None:
        with self.gen_fn(
                f'{union_name}::operator=',
                f'{union_name} &',
                params=[
                    f'{union_name} &&other'
                ],
        ):
            with self.gen_if('this != &other'):
                self.write('_type = other._type;')
                with self.type_declaration_options(
                        wrap_ptr=True,
                        wrap_nested_ptr=False,
                        string_ptr=True,
                        nested_string_ptr=False,
                ):
                    self.generate_union_move_body(enum_name, item_specs)
            self.write('return *this;')
        self.newline()

    @gensym_context
    def gen_union_copy_assign(
            self,
            union_symbol: LspSymbol,
            union_name: str,
            enum_name: str,
            item_specs: List[LspSpec]
    ) -> None:
        with self.gen_fn(
                f'{union_name}::operator=',
                f'{union_name} &',
                params=[
                    f'const {union_name} &other'
                ],
        ):
            with self.gen_if('this != &other'):
                self.write('_type = other._type;')
                with self.type_declaration_options(
                        wrap_ptr=True,
                        wrap_nested_ptr=False,
                        string_ptr=True,
                        nested_string_ptr=False,
                ):
                    self.generate_union_copy_body(
                        union_symbol,
                        enum_name,
                        item_specs
                    )
            self.write('return *this;')
        self.newline()

    def gen_union_enum_names(self, enum_name: str, item_specs: List[LspSpec]) -> None:
        names_by_enum = f'{enum_name}Names'
        with self.gen_map(names_by_enum, enum_name, 'std::string'):
            self.gen_enum_map_entry(enum_name, 'Uninitialized', 'Uninitialized')
            for index, item_spec in enumerate(item_specs):
                with self.nest_name(str(index)):
                    self.gen_enum_map_entry(enum_name, item_spec, item_spec)
        self.newline()

    def gen_debug_check(self, field_spec: LspSpec) -> bool:
        match field_spec["kind"]:
            case "base":
                return self._type_declaration_string_ptr \
                    and self.resolve(field_spec["name"]).normalized_name == "string"
            case "reference":
                field_symbol = self.resolve(field_spec["name"])
                match field_symbol.kind:
                    case LspSymbolKind.BASE:
                        return self._type_declaration_string_ptr \
                            and field_symbol.normalized_name == "string"
                    case LspSymbolKind.ENUMERATION:
                        return False
                    case _:
                        return self._type_declaration_wrap_ptr
            case _:
                return self._type_declaration_wrap_ptr

    @gensym_context
    def gen_union_field_copy_assign(
            self,
            union_symbol: LspSymbol,
            union_name: str,
            enum_name: str,
            field_name: str,
            field_spec: LspSpec,
            has_pointer: bool
    ) -> None:
        field_type = self.get_type_declaration(field_spec)
        match field_spec['kind']:
            case 'base' | 'reference':
                field_symbol = self.resolve(field_spec['name'])
                match field_symbol.kind:
                    case LspSymbolKind.BASE:
                        if field_symbol.normalized_name == "string":
                            params = [f'const {field_type} &{field_name}']
                        else:
                            params = [f'{field_type} {field_name}']
                    case LspSymbolKind.ENUMERATION:
                        params = [f'{field_type} {field_name}']
                    case _:
                        params = [f'const {field_type} &{field_name}']
            case _:
                params = [f'const {field_type} &{field_name}']
        with self.gen_fn(
                f'{union_name}::operator=',
                f'{union_name} &',
                params=params,
        ):
            if has_pointer:
                with self.gen_if_ne('_type', f'{enum_name}::Uninitialized'):
                    self.gen_call('reset')
            if self.gen_debug_check(field_spec):
                with self.gen_if_debug_enabled(f'!{field_name}'):
                    self.gen_throw(
                        'std::logic_error',
                        f'"null passed as the value of {field_name}"'
                    )
            self.generate_union_copy_field(union_symbol, field_spec)
            self.gen_assign('_type', self.get_enum_value(enum_name, field_spec))
            self.write('return *this;')
        self.newline()

    @gensym_context
    def gen_union_field_move_assign(
            self,
            union_name: str,
            enum_name: str,
            field_name: str,
            field_spec: LspSpec,
            has_pointer: bool
    ) -> None:
        field_type = self.get_type_declaration(field_spec)
        with self.gen_fn(
                f'{union_name}::operator=',
                f'{union_name} &',
                params=[
                    f'{field_type} &&{field_name}'
                ],
        ):
            if has_pointer:
                with self.gen_if_ne('_type', f'{enum_name}::Uninitialized'):
                    self.gen_call('reset')
            if self.gen_debug_check(field_spec):
                with self.gen_if_debug_enabled(f'!{field_name}'):
                    self.gen_throw(
                        'std::logic_error',
                        f'"null passed as the value of {field_name}"'
                    )
            match field_spec["kind"]:
                case "base" | "reference":
                    type_name = field_spec["name"]
                case "literal" | "array" | "tuple" | "or" | "map":
                    type_name = self.nested_name()
                case _:
                    raise ValueError(f'Unsupported union type ({field_spec["kind"]}): {field_spec}')
            type_symbol = self.resolve(type_name)
            type_name = rename_type(type_name)
            is_pointer_type = True
            match type_symbol.kind:
                case LspSymbolKind.BASE:
                    is_pointer_type = (type_symbol.normalized_name == "string")
                case LspSymbolKind.ENUMERATION:
                    is_pointer_type = False
            if is_pointer_type:
                self.update_union_field(
                    f'_{field_name}', type_name,
                    (f'std::move({field_name})'
                     if self._type_declaration_wrap_ptr
                     else f'std::make_unique<{type_name}>(std::move({field_name}))')
                )
            else:
                self.gen_assign(field_name, f'other.{field_name}')
            self.gen_assign_enum('_type', enum_name, field_spec)
            self.write('return *this;')
        self.newline()

    def gen_union_reset(self, union_symbol: LspSymbol, enum_name: str) -> None:
        union_name = union_symbol.name
        union_spec = union_symbol.spec
        field_specs = union_spec["items"]
        with self.gen_fn(f'{union_name}::reset', 'void'):
            has_pointer = any((self.is_union_pointer_type(field_spec)
                               for field_spec in field_specs))
            if has_pointer:
                with self.gen_switch('_type'):
                    with self.gen_case(enum_name, 'Uninitialized'):
                        self.write('// nothing to do')
                        self.write('break;')
                    for field_index, field_spec in enumerate(field_specs):
                        with self.nest_name(str(field_index)):
                            if self.is_union_pointer_type(field_spec):
                                field_name = self.name_field(field_spec)
                                field_type = self.get_type_declaration(field_spec)
                                with self.gen_case(enum_name, field_spec):
                                    with self.gen_if_debug_enabled(f'!_{field_name}'):
                                        self.gen_throw(
                                            'std::logic_error',
                                            f'"Attempted to free a null pointer to _{field_name}"'
                                        )
                                    # self.write(f'_{field_name}.reset();')
                                    self.write(f'_{field_name}.~unique_ptr<{field_type}>();')
                                    self.gen_assign('_type', f'{enum_name}::Uninitialized')
                                    self.gen_break()
                    with self.gen_default():
                        self.write(f'_type = {enum_name}::Uninitialized;')
            else:
                self.write(f'_type = {enum_name}::Uninitialized;')
        self.newline()

    def generate_union_additional_pointer_setters(
            self,
            union_symbol: LspSymbol,
            union_name: str,
            enum_name: str,
            field_name: str,
            field_spec: LspSpec,
            has_pointer: bool
    ) -> None:
        with self.type_declaration_options(
                wrap_ptr=True,
                wrap_nested_ptr=False,
                string_ptr=True,
                nested_string_ptr=False
        ):
            self.gen_union_field_move_assign(
                union_name,
                enum_name,
                field_name,
                field_spec,
                has_pointer
            )
        with self.type_declaration_options(
                wrap_ptr=False,
                wrap_nested_ptr=False,
                string_ptr=False,
                nested_string_ptr=False
        ):
            self.gen_union_field_copy_assign(
                union_symbol,
                union_name,
                enum_name,
                field_name,
                field_spec,
                has_pointer
            )
            self.gen_union_field_move_assign(
                union_name,
                enum_name,
                field_name,
                field_spec,
                has_pointer
            )

    def generate_union(self, union_symbol: LspSymbol) -> None:
        union_spec = union_symbol.spec
        union_name = union_symbol.name
        item_specs = union_spec["items"]
        enum_name = f'{union_name}Type'
        has_pointer = any((self.is_union_pointer_type(item_spec)
                           for item_spec in item_specs))

        self.gen_union_enum_names(enum_name, item_specs)
        self.gen_union_constructor(union_name)
        self.gen_union_destructor(union_name, has_pointer)
        self.gen_union_move_constructor(union_name, enum_name, item_specs)
        self.gen_union_copy_constructor(union_symbol, union_name, enum_name, item_specs)
        self.gen_union_move_assign(union_name, enum_name, item_specs)
        self.gen_union_copy_assign(union_symbol, union_name, enum_name, item_specs)
        if has_pointer:
            self.gen_union_reset(union_symbol, enum_name)
        self.generate_union_tag_getter(union_name)

        for item_index, item_spec in enumerate(item_specs):
            with self.nest_name(str(item_index)):
                field_name = self.name_field(item_spec)
                self.generate_union_field_getter(
                    union_name,
                    enum_name,
                    field_name,
                    item_spec
                )
                with self.type_declaration_options(
                        wrap_ptr=True,
                        wrap_nested_ptr=False,
                        string_ptr=True,
                        nested_string_ptr=False
                ):
                    self.gen_union_field_copy_assign(
                        union_symbol,
                        union_name,
                        enum_name,
                        field_name,
                        item_spec,
                        has_pointer
                    )
                if self.is_union_pointer_type(item_spec):
                    self.generate_union_additional_pointer_setters(
                        union_symbol,
                        union_name,
                        enum_name,
                        field_name,
                        item_spec,
                        has_pointer
                    )

    def generate_code(self) -> None:
        print(f'Generating: {self.file_path}')
        self.generate_disclaimer()
        self.gen_include('stdexcept')
        self.newline()
        self.gen_include('server/lsp_specification.h')
        self.newline()
        with self.gen_namespace(self.namespace):
            self.newline()
            super().generate_code()
            self.generate_request_enums()
            self.generate_notification_enums()
