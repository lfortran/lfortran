import re
from collections import defaultdict, deque
from contextlib import contextmanager
from functools import wraps
from pathlib import Path
from typing import Any, Callable, Dict, Iterator, List, Optional, Tuple, Union

from llanguage_server.cxx.lsp_file_generator import gensym_context
from llanguage_server.cxx.visitors import BaseCPlusPlusLspVisitor
from llanguage_server.lsp.datatypes import LspSpec, LspSymbol
from llanguage_server.lsp.utils import (any_enum, lower_first,
                                        method_to_camel_case, rename_enum,
                                        rename_field, rename_type, upper_first)
from llanguage_server.lsp.visitors import (LspAnalysisPipeline, LspSymbol,
                                           LspSymbolKind)

RE_VOWEL: re.Pattern = re.compile(r'^[aeiouy]', re.IGNORECASE)


SpecTypesAndTypeSpecs = List[
    Tuple[str, LspSpec]
]

MapTypesAndTypeSpecs = List[
    Tuple[
        str,
        LspSpec,
        Dict[str, List[Any]],
        Dict[str, List[Any]]
    ]
]

ArraySpecTypesAndTypeSpecs = List[
    Tuple[
        str,
        LspSpec,
        Dict[str, List[Any]]
    ]
]


def type_symbol_or_spec_or_name_fn(fn: Callable) -> Callable:
    memo = {}
    @wraps(fn)
    def wrapper(
            self,
            type_symbol_or_spec_or_name: Union[LspSymbol, LspSpec, str],
            *args,
            **kwargs
    ):
        match type_symbol_or_spec_or_name:
            case LspSymbol():
                type_symbol: LspSymbol = type_symbol_or_spec_or_name
                type_name = type_symbol.name
            case dict():
                type_spec: LspSpec = type_symbol_or_spec_or_name
                type_name = self.get_type_name(type_spec)
            case str():
                type_name: str = type_symbol_or_spec_or_name
        if type_name in memo:
            return memo[type_name]
        retval = fn(self, type_name, *args, **kwargs)
        memo[type_name] = retval
        return retval
    return wrapper


class CPlusPlusLspTransformerSourceGenerator(BaseCPlusPlusLspVisitor):

    def __init__(
            self,
            output_dir: Path,
            schema: LspSpec,
            pipeline: LspAnalysisPipeline,
            namespace: str
    ) -> None:
        specification_source = output_dir / "lsp_transformer.cpp"
        super().__init__(specification_source, schema, pipeline, namespace)

    def gen_as_any(self, value: str, move: bool = True) -> str:
        any_name = self.gensym_init('any', 'std::make_unique<LSPAny>()')
        expr = f'std::move({value})' if move else value
        self.gen_assign(f'(*{any_name})', expr)
        return any_name

    def move_as_any(self, expr: str, move_expr: bool = True) -> str:
        any_name = self.gen_as_any(expr, move=move_expr)
        return f'std::move({any_name})'

    def gen_return_as_any(self, value: str, move: bool = True) -> None:
        any_name = self.gensym_decl('LSPAny', 'any')
        expr = f'std::move({value})' if move else value
        self.gen_assign(any_name, expr)
        self.gen_return(any_name)

    def article_for(self, name: str) -> str:
        return 'an' if RE_VOWEL.match(name) else 'a'

    @contextmanager
    def gen_any_to_type(
            self,
            type_symbol_or_spec_or_name: Union[LspSymbol, LspSpec, str],
            return_type_or_spec: Optional[Union[str, LspSpec]] = None
    ) -> Iterator:
        return_type: str
        match return_type_or_spec:
            case None:
                match type_symbol_or_spec_or_name:
                    case LspSymbol():
                        type_symbol: LspSymbol = type_symbol_or_spec_or_name
                        return_type = type_symbol.name
                    case dict():
                        type_spec: LspSpec = type_symbol_or_spec_or_name
                        return_type = self.get_type_name(type_spec)
                    case str():
                        return_type = type_symbol_or_spec_or_name
                    case _:
                        raise ValueError(
                            'Unsupported return type ({kind}): {spec}'.format(
                                kind=type(type_symbol_or_spec_or_name),
                                spec=type_symbol_or_spec_or_name,
                            )
                        )
            case dict():
                return_spec: LspSpec = return_type_or_spec
                return_type = self.get_type_declaration(return_spec)
            case str():
                return_type = return_type_or_spec
            case _:
                raise ValueError(
                    'Unsupported return type ({kind}): {spec}'.format(
                        kind=type(return_type_or_spec),
                        spec=return_type_or_spec,
                    )
                )
        any_to_type = self.get_any_to_type_name(type_symbol_or_spec_or_name)
        with self.gen_fn(
            f'LspTransformer::{any_to_type}',
            return_type,
            params=[
                'const LSPAny &any',
            ],
            specs='const'
        ):
            yield self
        self.newline()

    @contextmanager
    def gen_type_to_any(
            self,
            type_symbol_or_spec_or_name: Union[LspSymbol, LspSpec, str],
            type_param: str
    ) -> Iterator:
        type_to_any = self.get_type_to_any_name(type_symbol_or_spec_or_name)
        with self.gen_fn(
            f'LspTransformer::{type_to_any}',
            'LSPAny',
            params=[
                type_param,
            ],
            specs='const'
        ):
            yield self
        self.newline()

    def value_of(self, value: str, is_optional: bool) -> str:
        if is_optional:
            return f'{value}.value()'
        return value

    def deref(
            self,
            symbol_or_spec_or_name: Union[LspSymbol, LspSpec, str],
            value: str
    ) -> str:
        name: str
        symbol: LspSymbol
        match symbol_or_spec_or_name:
            case LspSymbol():
                symbol = symbol_or_spec_or_name
            case dict():
                spec: LspSpec = symbol_or_spec_or_name
                if "name" in spec:
                    name = spec["name"]
                else:
                    name = self.nested_name()
                symbol = self.resolve(name)
            case str():
                name = symbol_or_spec_or_name
                symbol = self.resolve(name)
            case _:
                raise ValueError(
                    'Unsupported specification type ({kind}): {spec}'.format(
                        kind=type(symbol_or_spec_or_name),
                        spec=symbol_or_spec_or_name,
                    )
                )
        if symbol.resolution is not None:
            symbol = symbol.resolution
        match symbol.kind:
            case LspSymbolKind.BASE:
                if symbol.normalized_name == "string":
                    return f'*{value}'
                return value
            case _:
                return f'*{value}'

    @type_symbol_or_spec_or_name_fn
    def get_any_to_type_name(self, type_name: str) -> str:
        upper_type = upper_first(type_name)
        return f'anyTo{upper_type}'

    @type_symbol_or_spec_or_name_fn
    def get_type_to_any_name(self, type_name: str) -> str:
        lower_type = lower_first(type_name)
        return f'{lower_type}ToAny'

    @gensym_context
    def gen_any_to_enum(self, enum_spec: LspSpec) -> None:
        enum_name = enum_spec["name"]
        type_spec = enum_spec["type"]
        type_name = type_spec["name"]
        value_type = rename_type(type_name)
        field_name = self.name_field(type_spec)
        lower_enum = lower_first(enum_name)
        enum_by_value = f'{lower_enum}ByValue'
        enumerator = rename_enum(type_name)
        with self.gen_any_to_type(enum_name):
            with self.gen_try():
                with self.gen_switch('any.type()'):
                    match type_name:
                        case "string":
                            with self.gen_case('LSPAnyType', 'string'):
                                self.gen_assign(f'const {value_type} &value', f'any.{field_name}()')
                                self.gen_return(f'{enum_by_value}(value)')
                        case "integer":
                            with self.gen_case('LSPAnyType', 'integer'):
                                self.gen_assign(f'{value_type} value', f'any.{field_name}()')
                                self.gen_return(f'{enum_by_value}(value)')
                            with self.gen_case('LSPAnyType', 'uinteger'):
                                self.gen_assign(
                                    f'{value_type} value',
                                    f'static_cast<{value_type}>(any.{field_name}())'
                                )
                                self.gen_return(f'{enum_by_value}(value)')
                        case "uinteger":
                            with self.gen_case('LSPAnyType', 'uinteger'):
                                self.gen_assign(f'{value_type} value', f'any.{field_name}()')
                                self.gen_return(f'{enum_by_value}(value)')
                            with self.gen_case('LSPAnyType', 'integer'):
                                self.gen_assign(
                                    f'{value_type} value',
                                    f'static_cast<{value_type}>(any.{field_name}())'
                                )
                                self.gen_return(f'{enum_by_value}(value)')
                        case _:
                            raise ValueError(f'Unsupported enumeration type ({type_name}): {enum_spec}')
                    with self.gen_default():
                        self.gen_throw_invalid_params(
                            f'("LSPAnyType for {self.article_for(enum_name)} "',
                            f' "{enum_name}"',
                            f' " must be of type LSPAnyType::{enumerator}"',
                            f' " but received LSPAnyType::" + LSPAnyTypeNames.at(any.type()))'
                        )
            with self.gen_catch('std::invalid_argument &e'):
                self.gen_throw_invalid_params('e.what()')

    @gensym_context
    def gen_enum_to_any(self, enum_spec: LspSpec) -> None:
        enum_name = enum_spec["name"]
        type_spec = enum_spec["type"]
        type_name = type_spec["name"]
        value_type = rename_type(type_name)
        enum_param =f'{enum_name} enumerator'
        with self.gen_type_to_any(enum_name, enum_param):
            any_name = self.gensym_decl('LSPAny', 'any')
            if self.resolve(type_name).normalized_name == "string":
                values_by_enum = f'{enum_name}Values'
                self.gen_assign(any_name, f'{values_by_enum}.at(enumerator)')
            else:
                self.gen_assign(any_name, f'static_cast<{value_type}>(enumerator)')
            self.gen_return(any_name)

    def generate_enumeration(self, enum_spec: LspSpec) -> None:
        self.gen_any_to_enum(enum_spec)
        self.gen_enum_to_any(enum_spec)

    def get_type_name(self, type_spec: LspSpec) -> str:
        match type_spec["kind"]:
            case "base" | "reference" | "enumeration":
                return type_spec["name"]
            case "literal" | "or" | "array" | "map" | "tuple":
                return self.nested_name()
            case _:
                raise ValueError(f'Cannot determine type name for: {type_spec}')

    def get_upper_type_name(self, type_spec: LspSpec) -> str:
        type_name = self.get_type_name(type_spec)
        return upper_first(type_name)

    def generate_upper_type_name(self, type_spec: LspSpec) -> None:
        self.inline(self.get_upper_type_name(type_spec))

    def get_lower_type_name(self, type_spec: LspSpec) -> str:
        type_name = self.get_type_name(type_spec)
        return lower_first(type_name)

    def generate_lower_type_name(self, type_spec: LspSpec) -> None:
        self.inline(self.get_lower_type_name(type_spec))

    def index_by_type(
            self,
            type_index: Dict[str, List[Any]],
            type_spec: LspSpec,
            level: int = 0,
            type_name: Optional[str] = None
    ) -> None:
        type_kind = type_spec["kind"]
        match type_kind:
            case "base":
                base_spec = type_spec
                base_name = base_spec["name"]
                if self.resolve(base_name).normalized_name == "string":
                    index = type_index["string"]
                else:
                    index = type_index[base_name]
                index.append(("base", base_spec))
            case "reference":
                if type_name is None:
                    type_name = type_spec["name"]
                symbol = self.resolve(type_name)
                with self.nested_name_as(symbol.name):
                    match type_name:
                        case "LSPAny" | "LSPObject":
                            index = type_index["structure"]
                            index.append((type_name, symbol.spec))
                        case _:
                            match symbol.kind:
                                case LspSymbolKind.STRUCTURE:
                                    struct_spec = type_spec
                                    if type_name is not None:
                                        struct_name = type_name
                                    else:
                                        struct_name = type_spec["name"]
                                    index = type_index["structure"]
                                    index.append((struct_name, struct_spec))
                                case LspSymbolKind.ENUMERATION:
                                    enum_spec = type_spec
                                    if type_name is not None:
                                        enum_name = type_name
                                    else:
                                        enum_name = enum_spec["name"]
                                    index = type_index["enumeration"]
                                    index.append((enum_name, enum_spec))
                                case _:
                                    self.index_by_type(type_index, symbol.spec, level, type_name)
            case "array":
                array_spec = type_spec
                elem_spec = array_spec["element"]
                elem_index = defaultdict(list)
                with self.nest_name('elem'):
                    self.index_by_type(elem_index, elem_spec, 1 + level)
                index = type_index["array"]
                array_name = self.nested_name()
                index.append((array_name, type_spec, elem_index))
            case "map":
                key_spec = type_spec["key"]
                key_index = defaultdict(list)
                with self.nest_name('key'):
                    self.index_by_type(key_index, key_spec, 1 + level)
                value_spec = type_spec["value"]
                value_index = defaultdict(list)
                with self.nest_name('value'):
                    self.index_by_type(value_index, value_spec, 1 + level)
                index = type_index["map"]
                map_name = self.nested_name()
                index.append((map_name, type_spec, key_index, value_index))
            case "and":
                raise ValueError(f'AND types are not supported: {type_spec}')
            case "or":
                union_spec = type_spec
                if level == 0:
                    for i, item_spec in enumerate(union_spec["items"]):
                        with self.nest_name(str(i)):
                            self.index_by_type(type_index, item_spec, 1 + level)
                else:
                    if type_name is None:
                        type_name = self.nested_name()
                    index = type_index['union']
                    index.append((type_name, union_spec))
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
                index = type_index["inner"]
                if type_name is not None:
                    inner_name = type_name
                else:
                    inner_name = self.nested_name()
                index.append((inner_name, type_spec))
            case "stringLiteral":
                index = type_index["string"]
                index.append(type_spec)
            case "integerLiteral":
                index = type_index["integer"]
                index.append(type_spec)
            case "booleanLiteral":
                index = type_index["boolean"]
                index.append(type_spec)
            case _:
                raise ValueError(
                    f'Unsupported index type ({type_kind}): {type_spec}'
                )

    def gen_rec_any_to_union_type(
            self,
            union_name: str,
            value_name: str,
            type_name: str,
            spec_types_and_type_specs: SpecTypesAndTypeSpecs
    ) -> None:
        if len(spec_types_and_type_specs) > 0:
            spec_type, type_spec = spec_types_and_type_specs[0]
            if spec_type == "enumeration":
                with self.gen_try():
                    enum_spec = type_spec
                    enum_name = enum_spec["name"]
                    upper_enum = upper_first(enum_name)
                    any_to_enum = f'anyTo{upper_enum}'
                    self.gen_assign(f'{value_name}', f'{any_to_enum}(any)')
                with self.gen_catch('LspException &/*e*/'):
                    self.gen_rec_any_to_union_type(
                        union_name,
                        value_name,
                        type_name,
                        spec_types_and_type_specs[1:]
                    )
            elif len(spec_types_and_type_specs) == 1:
                upper_name = upper_first(type_name)
                any_to_type = f'anyTo{upper_name}'
                self.gen_assign(f'{value_name}', f'{any_to_type}(any)')
            else:
                raise ValueError(f'Redundant {type_name} specs detected')
        else:
            self.gen_throw_invalid_params(
                f'"Failed to transform LSPAny to {union_name}"'
            )

    def gen_rec_any_to_union_inner(
            self,
            union_name: str,
            value_name: str,
            struct_specs: Optional[SpecTypesAndTypeSpecs],
            inner_specs: Optional[SpecTypesAndTypeSpecs],
            union_specs: Optional[SpecTypesAndTypeSpecs],
            map_specs: Optional[MapTypesAndTypeSpecs],
            array_specs: Optional[ArraySpecTypesAndTypeSpecs]
    ) -> None:
        type_name: Optional[str] = None

        if struct_specs is not None and len(struct_specs) > 0:
            type_name, _ = struct_specs[0]
            struct_specs = struct_specs[1:]
        elif inner_specs is not None and len(inner_specs) > 0:
            type_name, _ = inner_specs[0]
            inner_specs = inner_specs[1:]
        elif union_specs is not None and len(union_specs) > 0:
            type_name, _ = union_specs[0]
            union_specs = union_specs[1:]
        elif map_specs is not None and len(map_specs) > 0:
            type_name, _, _, _ = map_specs[0]
            map_specs = map_specs[1:]
        elif array_specs is not None and len(array_specs) > 0:
            type_name, _, _ = array_specs[0]
            array_specs = array_specs[1:]

        if type_name is not None:
            with self.gen_try():
                any_to_type = self.get_any_to_type_name(type_name)
                self.gen_assign(f'{value_name}', f'{any_to_type}(any)')
            with self.gen_catch('LspException &/*e*/'):
                self.gen_rec_any_to_union_inner(
                    union_name,
                    value_name,
                    struct_specs,
                    inner_specs,
                    union_specs,
                    map_specs,
                    array_specs
                )
        else:
            self.gen_throw_invalid_params(
                f'"Failed to transform LSPAny to {union_name}"'
            )

    @gensym_context
    def generate_any_to_union(self, union_symbol: LspSymbol) -> None:
        union_spec = union_symbol.spec
        union_name = union_symbol.name
        type_index = defaultdict(list)
        self.index_by_type(type_index, union_spec)
        struct_specs = type_index.get("structure", None)
        inner_specs = type_index.get("inner", None)
        union_specs = type_index.get("union", None)
        map_specs = type_index.get("map", None)
        has_object_type = (struct_specs is not None) \
            or (inner_specs is not None) \
            or (union_specs is not None) \
            or (map_specs is not None)
        with self.gen_any_to_type(union_name, union_name):
            value_name = self.gensym_decl(union_name, 'value')
            self.newline()
            with self.gen_switch('any.type()'):
                if has_object_type:
                    with self.gen_case('LSPAnyType', 'object'):
                        self.gen_rec_any_to_union_inner(
                            union_name, value_name,
                            struct_specs, inner_specs, union_specs, map_specs,
                            None
                        )
                        self.gen_break()
                array_specs = type_index.get("array", None)
                if array_specs is not None:
                    with self.gen_case('LSPAnyType', 'array'):
                        self.gen_rec_any_to_union_inner(
                            union_name, value_name,
                            None, None, None, None,
                            array_specs
                        )
                        self.gen_break()
                string_specs = type_index.get("string", None)
                if string_specs is not None:
                    string_specs.sort(key=lambda pair: pair[0])
                    with self.gen_case('LSPAnyType', 'string'):
                        self.gen_rec_any_to_union_type(
                            union_name,
                            value_name,
                            'string',
                            string_specs
                        )
                        self.gen_break()
                integer_specs = type_index.get("integer", None)
                if integer_specs is not None:
                    with self.gen_case('LSPAnyType', 'integer'):
                        self.gen_rec_any_to_union_type(
                            union_name,
                            value_name,
                            'integer',
                            integer_specs
                        )
                        self.gen_break()
                uinteger_specs = type_index.get("uinteger", None)
                if uinteger_specs is not None:
                    with self.gen_case('LSPAnyType', 'uinteger'):
                        self.gen_rec_any_to_union_type(
                            union_name,
                            value_name,
                            'uinteger',
                            uinteger_specs
                        )
                        self.gen_break()
                decimal_specs = type_index.get("decimal", None)
                if decimal_specs is not None:
                    with self.gen_case('LSPAnyType', 'decimal'):
                        self.gen_assign(f'{value_name}', f'anyToDecimal(any)')
                        self.gen_break()
                boolean_specs = type_index.get("boolean", None)
                if boolean_specs is not None:
                    with self.gen_case('LSPAnyType', 'boolean'):
                        self.gen_assign(f'{value_name}', f'anyToBoolean(any)')
                        self.gen_break()
                null_specs = type_index.get("null", None)
                if null_specs is not None:
                    with self.gen_case('LSPAnyType', 'null'):
                        self.gen_assign(f'{value_name}', f'anyToNull(any)')
                        self.gen_break()
                with self.gen_default():
                    self.gen_throw_invalid_params(
                        f'("Invalid LSPAnyType for {self.article_for(union_name)} "',
                        f' "{union_name}"',
                        f' ": " + LSPAnyTypeNames.at(any.type()))'
                    )
            self.newline()
            self.gen_return(f'{value_name}')

    @gensym_context
    def generate_union_to_any(self, union_symbol: LspSymbol) -> None:
        union_spec = union_symbol.spec
        union_name = union_symbol.name
        enum_name = f'{union_name}Type'
        with self.gen_type_to_any(union_name, f'const {union_name} &value'):
            with self.gen_switch('value.type()'):
                for i, item_spec in enumerate(union_spec["items"]):
                    with self.nest_name(str(i)):
                        field_name = self.name_field(item_spec)
                        with self.gen_case(enum_name, item_spec):
                            match item_spec["kind"]:
                                case "array":
                                    array_spec = item_spec
                                    elem_spec = array_spec["element"]
                                    array_name = self.gensym_array(
                                        "LSPArray", "array",
                                        f'value.{field_name}().size()'
                                    )
                                    with self.gensym_foreach(f'value.{field_name}()') as elem_name:
                                        with self.nest_name('elem'):
                                            elem_to_any = self.get_type_to_any_name(elem_spec)
                                        elem_expr = f'{elem_to_any}({elem_name})'
                                        self.gen_call(
                                            f'{array_name}.push_back',
                                            self.move_as_any(elem_expr, move_expr=False)
                                        )
                                    any_name = self.gensym_decl('LSPAny', 'any')
                                    self.gen_assign(any_name, f'std::move({array_name})')
                                    self.gen_return(any_name)
                                case "tuple":
                                    tuple_spec = item_spec
                                    item_specs = tuple_spec["items"]
                                    array_name = self.gensym_array(
                                        "LSPArray", "array",
                                        str(len(item_specs))
                                    )
                                    tuple_type = self.get_type_declaration(tuple_spec)
                                    tuple_name = self.gensym('tuple')
                                    self.gen_assign(
                                        f'const {tuple_type} &{tuple_name}',
                                        f'value.{field_name}()'
                                    )
                                    for i, item_spec in enumerate(item_specs):
                                        lower_item = self.get_lower_type_name(item_spec)
                                        item_to_any = f'{lower_item}ToAny'
                                        elem_expr = f'{item_to_any}(std::get<{i}>({tuple_name}))'
                                        self.gen_call(
                                            f'{array_name}.push_back',
                                            self.move_as_any(elem_expr, move_expr=False)
                                        )
                                    any_name = self.gensym_decl('LSPAny', 'any')
                                    self.gen_assign(any_name, f'std::move({array_name})')
                                    self.gen_return(any_name)
                                case "reference":
                                    item_type_name = item_spec["name"]
                                    match item_type_name:
                                        case "LSPAny":
                                            self.gen_return(f'copy(value.{field_name}())')
                                        case "LSPObject" | "LSPArray":
                                            any_name = self.gensym_decl('LSPAny', 'any')
                                            self.gen_assign(any_name, f'copy(value.{field_name}())')
                                            self.gen_return(any_name)
                                        case _:
                                            lower_item = self.get_lower_type_name(item_spec)
                                            item_to_any = f'{lower_item}ToAny'
                                            self.gen_return(f'{item_to_any}(value.{field_name}())')
                                case _:
                                    lower_item = self.get_lower_type_name(item_spec)
                                    item_to_any = f'{lower_item}ToAny'
                                    self.gen_return(f'{item_to_any}(value.{field_name}())')
                with self.gen_default():
                    self.gen_throw_invalid_params(
                        f'("Unsupported {union_name}Type: " +',
                        f' {union_name}TypeNames.at(value.type()))'
                    )

    def generate_union(self, union_symbol: LspSymbol) -> None:
        self.generate_any_to_union(union_symbol)
        self.generate_union_to_any(union_symbol)

    def expand_fields(self, struct_spec_or_symbol_or_name: Union[LspSpec, LspSymbol, str]) -> Iterator[Tuple[str, LspSpec]]:
        struct_name: str
        struct_spec: LspSpec
        struct_symbol: LspSymbol
        match struct_spec_or_symbol_or_name:
            case dict():
                struct_spec = struct_spec_or_symbol_or_name
                if "name" in struct_spec:
                    struct_name = struct_spec["name"]
                else:
                    struct_name = self.nested_name()
                struct_symbol = self.resolve(struct_name)
                struct_spec = struct_symbol.spec
            case LspSymbol():
                struct_symbol = struct_spec_or_symbol_or_name
                struct_spec = struct_symbol.spec
                struct_name = struct_symbol.name
            case str():
                struct_name = struct_spec_or_symbol_or_name
                struct_symbol = self.resolve(struct_name)
                struct_spec = struct_symbol.spec
        extends_specs: Optional[List[LspSpec]] = struct_spec.get("extends", None)
        if extends_specs is not None:
            for extends_spec in extends_specs:
                super_name = extends_spec['name']
                super_symbol = self.resolve(super_name)
                yield from self.expand_fields(super_symbol)
        mixin_specs: Optional[List[LspSpec]] = struct_spec.get("mixins", None)
        if mixin_specs is not None:
            for mixin_spec in mixin_specs:
                mixin_name = mixin_spec['name']
                mixin_symbol = self.resolve(mixin_name)
                if mixin_symbol.resolution is not None:
                    mixin_symbol = mixin_symbol.resolution
                yield from self.expand_fields(mixin_symbol)
        if struct_symbol.fields is not None:
            for field_spec in struct_symbol.fields:
                yield struct_name, field_spec

    @gensym_context
    def generate_any_to_struct(self, struct_symbol: LspSymbol) -> None:
        struct_spec = struct_symbol.spec
        if "name" in struct_spec:
            struct_name = struct_spec["name"]
        else:
            struct_name = self.nested_name()
        with self.gen_any_to_type(struct_name, struct_name):
            enum_type = f'LSPAnyType::{rename_enum("object")}'
            with self.gen_if_ne('any.type()', enum_type):
                self.gen_throw_invalid_params(
                    f'("LSPAnyType for {self.article_for(struct_name)} "',
                    f' "{struct_name}"',
                    f' " must be of type {enum_type}"',
                    f' " but received LSPAnyType::" + LSPAnyTypeNames.at(any.type()))'
                )
            self.newline()
            value_name = self.gensym_decl(struct_name, 'value')
            self.newline()
            array_field = rename_field("array")
            object_field = rename_field("object")
            object_name = self.gensym_ref('object', f'any.{object_field}()')
            iter_name = self.gensym_decl('LSPObject::const_iterator', 'iter')
            type_names_and_field_specs = list(self.expand_fields(struct_spec))
            self.newline()
            num_fields = len(type_names_and_field_specs)
            with self.gen_if(f'{object_name}.size() > {num_fields}'):
                self.gen_throw_invalid_params(
                    f'("Too many attributes to transform to {self.article_for(struct_name)} "',
                    f' "{struct_name}"',
                    f' ": " + std::to_string({object_name}.size()) + " > {num_fields}")'
                )
            for field_type_name, field_spec in type_names_and_field_specs:
                field_name = field_spec["name"]
                with self.nested_names_as(deque([field_type_name, field_name])):
                    field_type_spec = field_spec["type"]
                    self.newline()
                    self.gen_assign(f'{iter_name}', f'{object_name}.find("{field_name}")')
                    with self.gen_if_ne(iter_name, f'{object_name}.end()', end=''):
                        match field_type_spec["kind"]:
                            case "base":
                                any_to_base = self.get_any_to_type_name(field_spec["type"])
                                field_expr = f'{any_to_base}(*{iter_name}->second)'
                            case "reference":
                                ref_type_name = self.get_type_name(field_spec["type"])
                                match ref_type_name:
                                    case "LSPAny":
                                        field_expr = f'copy(*{iter_name}->second)'
                                    case "LSPObject":
                                        field_expr = f'copy({iter_name}->second->{object_field}())'
                                    case _:
                                        any_to_ref = f'anyTo{ref_type_name}'
                                        field_expr = f'{any_to_ref}(*{iter_name}->second)'
                            case "literal" | "or":
                                nested_name = self.nested_name([field_type_name, field_name])
                                # nested_name = self.nested_name()
                                any_to_nested = self.get_any_to_type_name(nested_name)
                                field_expr = f'{any_to_nested}(*{iter_name}->second)'
                            case "array":
                                array_spec = field_type_spec
                                elem_spec = array_spec["element"]
                                array_name = self.gensym_ref('array', f'{iter_name}->second->{array_field}()')
                                values_name = self.gensym_decl(array_spec, 'values')
                                with self.nest_name('elem'):
                                    any_to_elem = self.get_any_to_type_name(elem_spec)
                                with self.gensym_foreach(array_name) as elem_name:
                                    match elem_spec["kind"]:
                                        case "base":
                                            array_expr = f'{any_to_elem}(*{elem_name})'
                                        case "reference":
                                            match elem_spec["name"]:
                                                case "LSPAny":
                                                    array_expr = f'copy(*{elem_name})'
                                                case "LSPObject":
                                                    array_expr = f'{elem_name}->{object_field}()'
                                                case _:
                                                    array_expr = f'{any_to_elem}(*{elem_name})'
                                        case "or" | "literal":
                                            # nested_name = self.nested_name([field_type_name, field_name])
                                            with self.nest_name('elem') as nested_name:
                                                if nested_name == "LSPAny":
                                                    array_expr = f'copy(*{elem_name})'
                                                else:
                                                    any_to_nested = self.get_any_to_type_name(nested_name)
                                                    array_expr = f'{any_to_nested}(*{elem_name})'
                                        case _:
                                            raise ValueError(
                                                f'Unsupported array type ({elem_spec["kind"]}): {elem_spec}'
                                            )
                                    if self.has_cycle(struct_symbol, elem_spec):
                                        elem_type = self.get_type_declaration(elem_spec)
                                        array_expr = f'std::make_unique<{elem_type}>({array_expr})'
                                    self.gen_call(f'{values_name}.push_back', array_expr)
                                field_expr = f'std::move({values_name})'
                            case "map":
                                map_spec = field_type_spec
                                value_spec = field_type_spec["value"]
                                map_type = self.get_type_declaration(map_spec)
                                map_name = self.gensym_decl(map_type, 'map')
                                with self.gensym_foreach_keyval(
                                        f'{iter_name}->second->{object_field}()'
                                ) as (map_key, map_value):
                                    match value_spec["kind"]:
                                        case "base" | "reference":
                                            with self.nest_name('value'):
                                                any_to_value = self.get_any_to_type_name(value_spec)
                                            map_expr = f'{any_to_value}(*{map_value})'
                                        case "array":
                                            array_spec = value_spec
                                            array_type = self.get_type_declaration(array_spec)
                                            array_name = self.gensym_decl(array_type, 'array')
                                            elem_spec = array_spec["element"]
                                            with self.nest_name('elem'):
                                                any_to_elem = self.get_any_to_type_name(elem_spec)
                                            with self.gensym_foreach(f'{map_value}->{array_field}()') as elem_name:
                                                self.gen_call(
                                                    f'{array_name}.push_back',
                                                    f'{any_to_elem}(*{elem_name})'
                                                )
                                            map_expr = f'std::move({array_name})'
                                        case "literal" | "or":
                                            with self.nest_name('value'):
                                                nested_name = self.nested_name()
                                                any_to_nested = self.get_any_to_type_name(nested_name)
                                                map_expr = f'{any_to_nested}(*{map_value})'
                                        case _:
                                            raise ValueError(
                                                f'Unsupported map value type ({value_spec["kind"]}): {value_spec}'
                                            )
                                    self.gen_call(f'{map_name}.emplace', map_key, map_expr)
                                field_expr = f'std::move({map_name})'
                            case "stringLiteral":
                                expected_value = field_type_spec["value"]
                                actual_value = self.gensym_ref('value', f'anyToString(*{iter_name}->second)')
                                with self.gen_if_ne(actual_value, f'"{expected_value}"'):
                                    self.gen_throw_invalid_params(
                                        f'("String value for "',
                                        f' "{struct_name}.{field_name}"',
                                        f' " must be \\"{expected_value}\\""',
                                        f' " but was: \\"{actual_value}\\"")'
                                    )
                                field_expr = actual_value
                            case _:
                                raise ValueError(
                                    'Unsupported property type ({kind}) for {struct}.{field}: {spec}'.format(
                                        kind=field_type_spec["kind"],
                                        struct=struct_name,
                                        field=field_name,
                                        spec=field_spec,
                                    )
                                )
                        if self.has_cycle(struct_symbol, field_spec["type"]):
                            field_type = self.get_type_declaration(field_spec["type"])
                            field_expr = f'std::make_unique<{field_type}>({field_expr})'
                        self.gen_assign(f'{value_name}.{field_name}', field_expr)
                    if not field_spec.get("optional", False):
                        with self.gen_else():
                            self.gen_throw_invalid_params(
                                f'"Missing required {struct_name} attribute: {field_name}"'
                            )
                    else:
                        self.inline(end='\n')
            self.newline()
            self.gen_return(value_name)

    def generate_struct_to_any_1(
            self,
            struct_symbol: LspSymbol,
            object_name: str,
            field_type_name: str,
            field_spec: LspSpec,
            field_value: str,
            is_optional: bool
    ) -> None:
        if is_optional:
            with self.gen_if(f'{field_value}.has_value()'):
                field_value = self.gensym_ref(
                    field_spec["name"],
                    f'{field_value}.value()'
                )
                if self.has_cycle(struct_symbol, field_spec["type"]):
                    field_value = f'*{field_value}'
                is_optional = False
                self.generate_struct_to_any_1(
                    struct_symbol,
                    object_name,
                    field_type_name,
                    field_spec,
                    field_value,
                    is_optional
                )
        else:
            field_name = field_spec["name"]
            field_type = field_spec["type"]
            match field_type["kind"]:
                case "base":
                    field_to_any = self.get_type_to_any_name(field_type)
                    field_expr = f'{field_to_any}({field_value})'
                    field_expr = f'std::make_unique<LSPAny>({field_expr})'
                case "reference":
                    match field_type["name"]:
                        case "LSPAny":
                            field_expr = f'copy({field_value})'
                        case "LSPObject" | "LSPArray":
                            any_name = self.gensym_decl('LSPAny', 'any')
                            self.gen_assign(any_name, f'copy({field_value})')
                            field_expr = f'std::move({any_name})'
                        case _:
                            field_to_any = self.get_type_to_any_name(field_type)
                            field_expr = f'{field_to_any}({field_value})'
                    field_expr = f'std::make_unique<LSPAny>({field_expr})'
                case "or" | "literal":
                    nested_name = self.nested_name()
                    nested_to_any = self.get_type_to_any_name(nested_name)
                    field_expr = f'{nested_to_any}({field_value})'
                    field_expr = f'std::make_unique<LSPAny>({field_expr})'
                case "array":
                    array_name = self.gensym_array(
                        'LSPArray', 'array',
                        f'{field_value}.size()'
                    )
                    elem_spec = field_type["element"]
                    with self.nest_name('elem'):
                        elem_to_any = self.get_type_to_any_name(elem_spec)
                    with self.gensym_foreach(field_value) as elem_name:
                        array_expr = elem_name
                        if self.has_cycle(struct_symbol, elem_spec):
                            array_expr = f'*{array_expr}'
                        match elem_spec["kind"]:
                            case "base" | "reference":
                                array_expr = f'{elem_to_any}({array_expr})'
                            case "or" | "literal":
                                with self.nest_name('elem') as nested_name:
                                    nested_to_any = self.get_type_to_any_name(nested_name)
                                    array_expr = f'{nested_to_any}({array_expr})'
                            case _:
                                raise ValueError(f'Unsupported array type ({elem_spec["kind"]}): {elem_spec}')
                        self.gen_call(
                            f'{array_name}.push_back',
                            self.move_as_any(array_expr, move_expr=False)
                        )
                    any_name = self.gen_as_any(array_name)
                    field_expr = f'std::move({any_name})'
                case "map":
                    map_spec = field_type
                    value_spec = map_spec["value"]
                    with self.nest_name('value'):
                        value_to_any = self.get_type_to_any_name(value_spec)
                    map_name = self.gensym_decl('LSPObject', 'map')
                    with self.gensym_foreach_keyval(field_value) as (key_name, value_name):
                        match value_spec["kind"]:
                            case "base" | "reference" | "or":
                                map_expr = f'{value_to_any}({value_name})'
                            case "array":
                                elem_spec = value_spec["element"]
                                with self.nest_name('elem'):
                                    elem_to_any = self.get_type_to_any_name(elem_spec)
                                array_name = self.gensym_decl('LSPArray', 'array')
                                with self.gensym_foreach(value_name) as elem_name:
                                    match elem_spec["kind"]:
                                        case "base":
                                            array_expr = f'{elem_to_any}({elem_name})'
                                        case "reference" | "or":
                                            array_expr = f'{elem_to_any}({elem_name})'
                                        case _:
                                            raise ValueError(
                                                f'Unsupported array type ({elem_spec["kind"]}): {elem_spec}'
                                            )
                                    self.gen_call(
                                        f'{array_name}.push_back',
                                        self.move_as_any(array_expr, move_expr=False)
                                    )
                                map_expr = array_name
                            case _:
                                raise ValueError(f'Unsupported map type ({value_spec["kind"]}): {value_spec}')
                        map_expr = self.move_as_any(map_expr, move_expr=False)
                        self.gen_call(f'{map_name}.emplace', key_name, map_expr)
                    field_expr = self.move_as_any(map_name)
                case "stringLiteral":
                    field_expr = f'stringToAny({field_value})'
                    field_expr = f'std::make_unique<LSPAny>({field_expr})'
                case _:
                    raise ValueError(f'Unsupported type ({field_type["kind"]}): {field_spec}')
            # if self.has_cycle(struct_symbol, field_type):
            #     field_expr = f'std::make_unique<{field_type_name}>({field_expr})'
            self.gen_call(f'{object_name}.emplace', f'"{field_name}"', field_expr)

    @gensym_context
    def generate_struct_to_any(self, struct_symbol: LspSymbol) -> None:
        struct_spec = struct_symbol.spec
        if "name" in struct_spec:
            struct_name = struct_spec["name"]
        else:
            struct_name = self.nested_name()
        field_types_and_specs = list(self.expand_fields(struct_spec))
        param_name = 'param'
        if len(field_types_and_specs) == 0:
            param_name = f'/*{param_name}*/'
        with self.gen_type_to_any(struct_name, f'const {struct_name} &{param_name}'):
            object_name = self.gensym_decl('LSPObject', 'object')
            self.newline()
            for field_type_name, field_spec in field_types_and_specs:
                field_name = field_spec["name"]
                with self.nested_names_as(deque([field_type_name, field_name])):
                    is_optional = field_spec.get("optional", False)
                    field_value = f'{param_name}.{field_name}'
                    self.generate_struct_to_any_1(
                        struct_symbol,
                        object_name,
                        field_type_name,
                        field_spec,
                        field_value,
                        is_optional
                    )
                    self.newline()
            self.gen_return_as_any(object_name)

    def generate_structure(self, struct_symbol: LspSymbol) -> None:
        with self.nested_name_as(struct_symbol.name):
            self.generate_any_to_struct(struct_symbol)
            self.generate_struct_to_any(struct_symbol)

    def generate_any_to_array_body(self, array_spec: LspSpec) -> None:
        elem_spec = array_spec["element"]
        array_field = rename_field("array")
        array_name = self.gensym_ref('array', f'any.{array_field}()')
        values_name = self.gensym_array(
            array_spec, 'values',
            f'{array_name}.size()'
        )
        with self.gensym_foreach(array_name) as elem_name:
            elem_expr = elem_name
            array_name = self.nested_name()
            match elem_spec["kind"]:
                case "base" | "reference" | "literal" | "or":
                    with self.nest_name('elem'):
                        any_to_elem = self.get_any_to_type_name(elem_spec)
                    elem_expr = f'{any_to_elem}(*{elem_expr})'
                case _:
                    raise ValueError(
                        f'Unsupported array type ({elem_spec["kind"]}): {elem_spec}'
                    )
            if self.has_cycle(array_name, elem_spec):
                elem_type = self.get_type_declaration(elem_spec)
                elem_expr = f'std::make_unique<{elem_type}>({elem_expr})'
            self.gen_call(f'{values_name}.push_back', elem_expr)
        self.gen_return(values_name)

    @gensym_context
    def generate_any_to_array(self, array_symbol: LspSymbol) -> None:
        array_name = array_symbol.name
        with self.gen_any_to_type(array_name, array_name):
            array_spec = array_symbol.spec
            self.generate_any_to_array_body(array_spec)

    @gensym_context
    def generate_any_to_alias(self, alias_symbol: LspSymbol) -> None:
        alias_name = alias_symbol.name
        alias_spec = alias_symbol.spec
        resolution = alias_symbol
        if alias_symbol.resolution is not None:
            resolution = alias_symbol.resolution
        with self.gen_any_to_type(alias_name, alias_name):
            type_spec = alias_spec["type"]
            match type_spec["kind"]:
                case "base":
                    type_enum = any_enum(resolution.spec['name'])
                    with self.gen_switch('any.type()'):
                        uinteger_field = rename_field('uinteger')
                        integer_field = rename_field('integer')
                        uinteger_type = rename_type('uinteger')
                        integer_type = rename_type('integer')
                        decimal_type = rename_type('decimal')
                        with self.gen_case('LSPAnyType', type_enum):
                            field_name = rename_field(resolution.normalized_name)
                            self.gen_return(f'any.{field_name}()')
                        match alias_name:
                            case "integer":
                                with self.gen_case('LSPAnyType', 'uinteger'):
                                    self.gen_return(
                                        f'static_cast<{integer_type}>(any.{uinteger_field}())'
                                    )
                            case "uinteger":
                                with self.gen_case('LSPAnyType', 'integer'):
                                    self.gen_return(
                                        f'static_cast<{uinteger_type}>(any.{integer_field}())'
                                    )
                            case "decimal":
                                with self.gen_case('LSPAnyType', 'integer'):
                                    self.gen_return(
                                        f'static_cast<{decimal_type}>(any.{integer_field}())'
                                    )
                                with self.gen_case('LSPAnyType', 'uinteger'):
                                    self.gen_return(
                                        f'static_cast<{decimal_type}>(any.{uinteger_field}())'
                                    )
                        with self.gen_default():
                            self.gen_throw_invalid_params(
                                f'("Cannot transform LSPAny of type LSPAnyType::" +',
                                f' LSPAnyTypeNames.at(any.type()) +',
                                f' " to type {rename_type(alias_name)}")'
                            )
                case "reference":
                    any_to_type = self.get_any_to_type_name(type_spec)
                    self.gen_return(f'{any_to_type}(any)')
                case "array":
                    return self.generate_any_to_array_body(type_spec)
                case _:
                    raise ValueError(
                        f'Unsupported alias type ({type_spec["kind"]}): {type_spec}'
                    )

    def generate_array_to_any_body(self, source_array: str, array_spec: LspSpec) -> None:
        elem_spec = array_spec["element"]
        target_array = \
            self.gensym_array('LSPArray', 'array', f'{source_array}.size()')
        with self.gensym_foreach(source_array) as elem_name:
            elem_expr = elem_name
            array_name = self.nested_name()
            if self.has_cycle(array_name, elem_spec):
                elem_expr = f'*{elem_expr}'
            with self.nest_name('elem'):
                elem_to_any = self.get_type_to_any_name(elem_spec)
            elem_expr = f'{elem_to_any}({elem_expr})'
            elem_expr = self.move_as_any(elem_expr, move_expr=False)
            self.gen_call(f'{target_array}.push_back', elem_expr)
        self.gen_return_as_any(target_array)

    @gensym_context
    def generate_array_to_any(self, array_symbol: LspSymbol) -> None:
        array_name = array_symbol.name
        array_type = rename_type(array_name)
        with self.gen_type_to_any(
                array_name,
                f'const {array_type} &array'
        ):
            self.generate_array_to_any_body('array', array_symbol.spec)

    @gensym_context
    def generate_alias_to_any(self, alias_symbol: LspSymbol) -> None:
        alias_name = alias_symbol.name
        alias_type = rename_type(alias_name)
        with self.gen_type_to_any(alias_name, f'const {alias_type} &alias'):
            alias_spec = alias_symbol.spec
            type_spec = alias_spec["type"]
            match type_spec["kind"]:
                case "base":
                    self.gen_return_as_any('alias', move=False)
                case "reference":
                    type_to_any = self.get_type_to_any_name(type_spec)
                    self.gen_return(f'{type_to_any}(alias)')
                case "array":
                    self.generate_array_to_any_body('alias', type_spec)
                case _:
                    raise ValueError(
                        f'Unsupported alias type ({type_spec["kind"]}): {type_spec}'
                    )

    def visit_alias_symbol(self, alias_symbol: LspSymbol) -> None:
        with self.nested_name_as(alias_symbol.name):
            self.generate_any_to_alias(alias_symbol)
            self.generate_alias_to_any(alias_symbol)

    @gensym_context
    def generate_base_to_any(self, base_symbol: LspSymbol) -> None:
        base_name = base_symbol.name
        base_type = rename_type(base_name)
        if base_symbol.normalized_name == "string":
            base_param = f'const {base_type} &param'
        else:
            base_param = f'{base_type} param'
        with self.gen_type_to_any(base_name, base_param):
            self.gen_return_as_any('param', move=False)

    @gensym_context
    def generate_any_to_base(self, base_symbol: LspSymbol) -> None:
        base_name = base_symbol.name
        base_type = rename_type(base_name)
        integer_type = rename_type('integer')
        uinteger_type = rename_type('uinteger')
        decimal_type = rename_type('decimal')
        integer_field = rename_field('integer')
        uinteger_field = rename_field('uinteger')
        with self.gen_any_to_type(base_name, base_type):
            with self.gen_switch('any.type()'):
                with self.gen_case('LSPAnyType', base_name):
                    field_name = rename_field(base_symbol.resolution.normalized_name)
                    self.gen_return(f'any.{field_name}()')
                match base_name:
                    case 'integer':
                        with self.gen_case('LSPAnyType', 'uinteger'):
                            self.gen_return(
                                f'static_cast<{integer_type}>(any.{uinteger_field}())'
                            )
                    case 'uinteger':
                        with self.gen_case('LSPAnyType', 'integer'):
                            self.gen_return(
                                f'static_cast<{uinteger_type}>(any.{integer_field}())'
                            )
                    case 'decimal':
                        with self.gen_case('LSPAnyType', 'integer'):
                            self.gen_return(
                                f'static_cast<{decimal_type}>(any.{integer_field}())'
                            )
                        with self.gen_case('LSPAnyType', 'uinteger'):
                            self.gen_return(
                                f'static_cast<{decimal_type}>(any.{uinteger_field}())'
                            )
                with self.gen_default():
                    self.gen_throw_invalid_params(
                        f'("Cannot transform LSPAny of type LSPAnyType::" +',
                        f' LSPAnyTypeNames.at(any.type()) +',
                        f' " to type {base_type}")'
                    )

    def visit_base_symbol(self, base_symbol: LspSymbol) -> None:
        with self.nested_name_as(base_symbol.name):
            self.generate_base_to_any(base_symbol)
            self.generate_any_to_base(base_symbol)

    def visit_array_symbol(self, array_symbol: LspSymbol) -> None:
        with self.nested_name_as(array_symbol.name):
            self.generate_array_to_any(array_symbol)
            self.generate_any_to_array(array_symbol)

    @gensym_context
    def generate_map_to_any(self, map_symbol: LspSymbol) -> None:
        map_name = map_symbol.name
        map_type = rename_type(map_name)
        map_param = f'const {map_type} &map'
        with self.gen_type_to_any(map_name, map_param):
            map_spec = map_symbol.spec
            value_spec = map_spec['value']
            with self.nest_name('value'):
                value_to_any = self.get_type_to_any_name(value_spec)
            object_name = self.gensym_decl('LSPObject', 'object')
            with self.gensym_foreach_keyval('map') as (map_key, map_value):
                value_expr = map_value
                if self.has_cycle(map_symbol, value_spec):
                    value_expr = f'*{value_expr}'
                value_expr = f'{value_to_any}({value_expr})'
                value_expr = f'std::make_unique<LSPAny>({value_expr})'
                self.gen_call(f'{object_name}.emplace', map_key, value_expr)
            self.gen_return_as_any(object_name)

    @gensym_context
    def generate_any_to_map(self, map_symbol: LspSymbol) -> None:
        map_name = map_symbol.name
        with self.gen_any_to_type(map_name, map_name):
            map_spec = map_symbol.spec
            value_spec = map_spec['value']
            with self.nest_name('value'):
                any_to_value = self.get_any_to_type_name(value_spec)
            map_name = self.gensym_decl(map_spec, 'map')
            object_field = rename_field('object')
            with self.gensym_foreach_keyval(f'any.{object_field}()') as (map_key, map_value):
                value_expr = f'{any_to_value}(*{map_value})'
                if self.has_cycle(map_symbol, value_spec):
                    value_type = self.get_type_declaration(value_spec)
                    value_expr = f'std::make_unique<{value_type}>({value_expr})'
                self.gen_call(f'{map_name}.emplace', map_key, value_expr)
            self.gen_return(map_name)

    def visit_map_symbol(self, map_symbol: LspSymbol) -> None:
        with self.nested_name_as(map_symbol.name):
            self.generate_map_to_any(map_symbol)
            self.generate_any_to_map(map_symbol)

    @gensym_context
    def generate_copy_any(self) -> None:
        with self.gen_fn(
                'LspTransformer::copy',
                'LSPAny',
                params=[
                    'const LSPAny &any',
                ],
                specs="const"
        ):
            copy_name = self.gensym_decl('LSPAny', 'any')
            with self.gen_switch('any.type()'):
                with self.gen_case("LSPAnyType", "object"):
                    object_field = rename_field("LSPObject")
                    self.gen_assign(copy_name, f'copy(any.{object_field}())')
                    self.gen_break()
                with self.gen_case('LSPAnyType', 'array'):
                    array_field = rename_field("LSPArray")
                    self.gen_assign(copy_name, f'copy(any.{array_field}())')
                    self.gen_break()
                with self.gen_case('LSPAnyType', 'string'):
                    string_field = rename_field("string")
                    self.gen_assign(copy_name, f'any.{string_field}()')
                    self.gen_break()
                with self.gen_case('LSPAnyType', 'integer'):
                    integer_field = rename_field("integer")
                    self.gen_assign(copy_name, f'any.{integer_field}()')
                    self.gen_break()
                with self.gen_case('LSPAnyType', 'uinteger'):
                    uinteger_field = rename_field("uinteger")
                    self.gen_assign(copy_name, f'any.{uinteger_field}()')
                    self.gen_break()
                with self.gen_case('LSPAnyType', 'decimal'):
                    decimal_field = rename_field("decimal")
                    self.gen_assign(copy_name, f'any.{decimal_field}()')
                    self.gen_break()
                with self.gen_case('LSPAnyType', f'boolean'):
                    boolean_field = rename_field("boolean")
                    self.gen_assign(copy_name, f'any.{boolean_field}()')
                    self.gen_break()
                with self.gen_case('LSPAnyType', 'null'):
                    null_field = rename_field("null")
                    self.gen_assign(copy_name, f'any.{null_field}()')
                    self.gen_break()
                with self.gen_case('LSPAnyType', 'Uninitialized'):
                    self.gen_throw(
                        'std::invalid_argument',
                        '"LSPAny has not been initialized."'
                    )
            self.gen_return(copy_name)
        self.newline()

    @gensym_context
    def generate_copy_any_pointer(self) -> None:
        with self.gen_fn(
                'LspTransformer::copy',
                'std::unique_ptr<LSPAny>',
                params=[
                    'const std::unique_ptr<LSPAny> &any',
                ],
                specs="const"
        ):
            any_name = self.gensym_init('any', 'std::make_unique<LSPAny>()')
            self.gen_assign(f'(*{any_name})', 'copy(*any)')
            self.gen_return(any_name)
        self.newline()

    @gensym_context
    def generate_copy_object(self) -> None:
        with self.gen_fn(
                'LspTransformer::copy',
                'LSPObject',
                params=[
                    'const LSPObject &object',
                ],
                specs='const'
        ):
            copy_name = self.gensym_decl('LSPObject', 'copy')
            with self.gensym_foreach_keyval('object') as (key_name, value_name):
                self.gen_call(
                    f'{copy_name}.emplace',
                    key_name,
                    f'copy({value_name})'
                )
            self.gen_return(copy_name)
        self.newline()

    @gensym_context
    def generate_copy_array(self) -> None:
        with self.gen_fn(
                'LspTransformer::copy',
                'LSPArray',
                params=[
                    'const LSPArray &array',
                ],
                specs='const'
        ):
            copy_name = self.gensym_array('LSPArray', 'copy', 'array.size()')
            with self.gensym_foreach('array') as elem_name:
                self.gen_call(f'{copy_name}.push_back', f'copy({elem_name})')
            self.gen_return(copy_name)
        self.newline()

    def generate_copy_methods(self) -> None:
        self.generate_copy_any()
        self.generate_copy_any_pointer()
        self.generate_copy_object()
        self.generate_copy_array()

    def get_as_message_params_type(self, type_spec: LspSpec) -> str:
        match type_spec["kind"]:
            case "reference":
                return type_spec["name"]
            case _:
                raise ValueError(f'Unsupported request parameter type: {type_spec}')

    @gensym_context
    def generate_as_incoming_params(
        self,
        message_name: str,
        message_symbol: LspSymbol
    ) -> None:
        message_spec = message_symbol.spec
        params_spec = message_spec.get("params", None)
        if params_spec is not None:
            method_name = method_to_camel_case(message_spec["method"])
            type_name = f'{method_name}Params'
            as_message_params = f'as{type_name}'
            with self.gen_fn(
                    f'LspTransformer::{as_message_params}',
                    self.get_as_message_params_type(params_spec),
                    params=[
                        f'const MessageParams &{message_name}'
                    ],
                    specs='const'
            ):
                object_enum = rename_enum('object')
                object_field = rename_field('LSPObject')
                with self.gen_if_ne(f'{message_name}.type()', f'MessageParamsType::{object_enum}'):
                    self.gen_throw_invalid_params(
                        f'("Message parameter type must be MessageParamsType::{object_enum} "',
                        f' "for method=\\"{message_spec["method"]}\\" but received "',
                        f' "MessageParamsType::" + MessageParamsTypeNames.at({message_name}.type()))'
                    )
                self.newline()
                match params_spec["kind"]:
                    case "reference":
                        params_type = self.get_type_declaration(params_spec)
                        params_name = self.gensym_decl(params_type, 'params')
                        field_types_and_specs = list(self.expand_fields(params_spec))
                        if len(field_types_and_specs) > 0:
                            object_name = self.gensym_ref('object', f'{message_name}.{object_field}()')
                            iter_name = self.gensym_decl('LSPObject::const_iterator', 'iter')
                            self.newline()
                            for field_type_name, field_spec in field_types_and_specs:
                                field_name = field_spec["name"]
                                with self.nested_names_as([field_type_name, field_name]):
                                    field_type = field_spec["type"]
                                    self.gen_assign(f'{iter_name}', f'{object_name}.find("{field_name}")')
                                    with self.gen_if_ne(iter_name, f'{object_name}.end()', end=''):
                                        match field_type["kind"]:
                                            case "base":
                                                base_spec = field_type
                                                any_to_base = self.get_any_to_type_name(base_spec)
                                                params_expr = f'{any_to_base}(*{iter_name}->second)'
                                            case "reference":
                                                match field_type["name"]:
                                                    case "LSPAny":
                                                        params_expr = f'copy(*{iter_name}->second)'
                                                    case "LSPObject" | "LSPArray":
                                                        params_expr = f'copy(*{iter_name}->second)'
                                                    case _:
                                                        type_spec = field_type
                                                        any_to_type = self.get_any_to_type_name(type_spec)
                                                        params_expr = f'{any_to_type}(*{iter_name}->second)'
                                            case "array":
                                                array_spec = field_type
                                                elem_spec = array_spec["element"]
                                                array_field = rename_field('LSPArray')
                                                array_name = self.gensym_ref('array', f'{iter_name}->second->{array_field}()')
                                                values_name = self.gensym_decl(array_spec, 'values')
                                                self.write(f'{values_name}.reserve({array_name}.size());')
                                                with self.gensym_foreach(array_name) as elem_name:
                                                    match elem_spec["name"]:
                                                        case "LSPAny":
                                                            elem_expr = f'copy(*{elem_name})'
                                                        case "LSPObject":
                                                            elem_expr = f'copy({elem_name}->{object_field}())'
                                                        case "LSPArray":
                                                            elem_expr = f'copy({elem_name}->{array_field}())'
                                                        case _:
                                                            with self.nest_name('elem'):
                                                                any_to_elem = self.get_any_to_type_name(elem_spec)
                                                            elem_expr = f'{any_to_elem}(*{elem_name})'
                                                    self.gen_call(f'{values_name}.push_back', elem_expr)
                                                params_expr = f'std::move({values_name})'
                                            case "or" | "literal":
                                                nested_name = self.nested_name([field_type_name, field_name])
                                                any_to_nested = self.get_any_to_type_name(nested_name)
                                                params_expr = f'{any_to_nested}(*{iter_name}->second)'
                                            case _:
                                                raise ValueError(
                                                    f'Unsupported property kind ({field_type["kind"]}): {field_type}'
                                                )
                                        self.gen_assign(f'{params_name}.{field_name}', params_expr)
                                    if not field_spec.get("optional", False):
                                        with self.gen_else():
                                            self.gen_throw_invalid_params(
                                                f'"Missing required {params_spec["name"]} attribute: {field_name}"'
                                            )
                                    else:
                                        self.inline(end='\n')
                                    self.newline()
                        self.gen_return(params_name)
                    case _:
                        raise ValueError(
                            f'Unsupported message parameter type ({params_spec["name"]}): {params_spec}'
                        )
            self.newline()

    def generate_result_union_array_union_to_any(
            self,
            result_name: str,
            array_name: str,
            union_spec: LspSpec,
            index: int
    ) -> None:
        union_name = self.nested_name([result_name, str(index)])
        field_name = rename_field(union_name)
        field_value = f'result.{field_name}()'
        self.gen_call(
            f'{array_name}.reserve',
            f'{field_value}.size()'
        )
        union_enum = f'{union_name}Type'
        with self.gensym_foreach(field_value) as elem_name:
            with self.gen_switch(f'{elem_name}.type()'):
                for union_item in union_spec["items"]:
                    with self.gen_case(union_enum, union_item):
                        match union_item["kind"]:
                            case "base" | "reference":
                                item_field = self.name_field(union_item)
                                item_name = union_item["name"]
                                match item_name:
                                    case "LSPAny" | "LSPObject" | "LSPArray":
                                        item_field = rename_field(item_name)
                                        elem_expr = f'copy({elem_name}.{item_field}())'
                                    case _:
                                        item_to_any = self.get_type_to_any_name(union_item)
                                        elem_expr = f'{item_to_any}({elem_name}.{item_field}())'
                            case _:
                                raise ValueError(
                                    f'Unsupported item kind ({union_item["kind"]}): {union_item}'
                                )
                        self.gen_call(f'{array_name}.push_back', elem_expr)
                        self.gen_break()
                with self.gen_case(union_enum, 'Uninitialized'):
                    self.gen_throw(
                        'LSP_EXCEPTION',
                        'ErrorCodes::InternalError',
                        f'"{result_name} has not been initialized!"'
                    )

    def generate_result_union_array_to_any(
            self,
            result_name: str,
            array_spec: LspSpec,
            index: int
    ) -> None:
        field_name = self.name_field(array_spec)
        elem_spec = array_spec["element"]
        array_name = self.gensym_decl('LSPArray', 'array')
        match elem_spec["kind"]:
            case "base" | "reference":
                field_value = f'result.{field_name}()'
                self.gen_call(
                    f'{array_name}.reserve',
                    f'{field_value}.size()'
                )
                with self.gensym_foreach(field_value) as elem_name:
                    with self.nest_name('elem'):
                        elem_to_any = self.get_type_to_any_name(elem_spec)
                    elem_expr = f'{elem_to_any}({elem_name})'
                    self.gen_call(
                        f'{array_name}.push_back',
                        self.move_as_any(elem_expr, move_expr=False)
                    )
            case "or":
                self.generate_result_union_array_union_to_any(
                    result_name,
                    array_name,
                    elem_spec,
                    index
                )
            case _:
                raise ValueError(f'Unsupported array type ({elem_spec["kind"]}): {elem_spec}')
        self.gen_return_as_any(array_name)

    def generate_result_union_reference_to_any(
            self,
            reference_spec: LspSpec,
            field_expr: str
    ) -> None:
        reference_to_any = self.get_type_to_any_name(reference_spec)
        reference_name = reference_spec["name"]
        match reference_name:
            case "LSPAny" | "LSPObject" | "LSPArray":
                self.gen_return(f'copy({field_expr})')
            case _:
                reference_symbol = self.resolve(reference_name)
                match reference_symbol.kind:
                    case LspSymbolKind.BASE:
                        if reference_symbol.normalized_name == "string":
                            field_expr = f'*{field_expr}'
                    case LspSymbolKind.STRUCTURE | \
                         LspSymbolKind.INNER | \
                         LspSymbolKind.UNION:
                        field_expr = f'*{field_expr}'
                self.gen_return(f'{reference_to_any}({field_expr})')

    def generate_result_union_to_any(
            self,
            result_type: str,
            result_name: str,
            symbol_spec: LspSpec
    ) -> None:
        with self.gen_switch('result.type()'):
            for index, item_spec in enumerate(symbol_spec["items"]):
                with self.gen_case(result_type, item_spec):
                    match item_spec["kind"]:
                        case "base":
                            field_name = self.name_field(item_spec)
                            item_to_any = self.get_type_to_any_name(item_spec)
                            self.gen_return(f'{item_to_any}(result.{field_name}())')
                        case "reference":
                            reference_spec = item_spec
                            reference_name = reference_spec["name"]
                            field_name = rename_field(reference_name)
                            field_expr = f'result.{field_name}()'
                            self.generate_result_union_reference_to_any(
                                reference_spec,
                                field_expr
                            )
                        case "array":
                            with self.nest_name('elem'):
                                self.generate_result_union_array_to_any(
                                    result_name,
                                    item_spec,
                                    index
                                )
                        case _:
                            raise ValueError(f'Unsupported union type ({item_spec["kind"]}): {item_spec}')
            with self.gen_case(result_type, 'Uninitialized'):
                self.gen_throw(
                    'LSP_EXCEPTION',
                    'ErrorCodes::InternalError',
                    f'"{result_name} has not been initialized!"'
                )
        self.write('throw LSP_EXCEPTION(')
        with self.indent():
            self.write('ErrorCodes::InternalError,')
            self.write('"Should be unreachable."')
        self.write(');')

    def generate_result_array_to_any(self, array_spec: LspSpec) -> None:
        elem_spec = array_spec["element"]
        array_name = self.gensym_array('LSPArray', 'array', 'result.size()')
        with self.gensym_foreach('result') as elem_name:
            with self.nest_name('elem'):
                elem_to_any = self.get_type_to_any_name(elem_spec)
            elem_expr = f'{elem_to_any}({elem_name})'
            self.gen_call(
                f'{array_name}.push_back',
                self.move_as_any(elem_expr, move_expr=False)
            )
        self.gen_return_as_any(array_name)

    @gensym_context
    def generate_result_to_any(self, request_spec: LspSpec) -> None:
        request_method = request_spec["method"]
        request_name = method_to_camel_case(request_method)
        result_name = f'{request_name}Result'
        with self.nested_names_as(deque([result_name])):
            result_type = f'{result_name}Type'
            symbol = self.resolve(result_name)
            symbol_spec = symbol.spec
            with self.gen_type_to_any(result_name, f'const {result_name} &result'):
                match symbol.kind:
                    case LspSymbolKind.UNION:
                        self.generate_result_union_to_any(
                            result_type,
                            result_name,
                            symbol_spec
                        )
                    case LspSymbolKind.BASE | LspSymbolKind.REFERENCE:
                        type_to_any = self.get_type_to_any_name(symbol)
                        self.gen_return(f'{type_to_any}(result)')
                    case LspSymbolKind.ARRAY:
                        self.generate_result_array_to_any(symbol_spec)
                    case _:
                        raise ValueError(f'Unsupported symbol kind ({symbol.kind}): {symbol}')

    def gen_struct_as_outgoing_params(
            self,
            object_name: str,
            field_name: str,
            field_spec: LspSpec,
            field_value: str,
            is_optional: bool
    ) -> None:
        if is_optional:
            with self.gen_if(f'{field_value}.has_value()'):
                is_optional = False
                field_value = f'{field_value}.value()'
                self.gen_struct_as_outgoing_params(
                    object_name,
                    field_name,
                    field_spec,
                    field_value,
                    is_optional
                )
        else:
            field_type = field_spec["type"]
            match field_type["kind"]:
                case 'base' | 'reference' | 'or' | 'literal':
                    type_to_any = self.get_type_to_any_name(field_type)
                    field_expr = f'{type_to_any}({field_value})'
                case 'array':
                    array_spec = field_type
                    elem_spec: LspSpec = array_spec['element']
                    with self.nest_name('elem'):
                        elem_to_any = self.get_type_to_any_name(elem_spec)
                    array_name = self.gensym_array(
                        'LSPArray', 'array',
                        f'{field_value}.size()'
                    )
                    with self.gensym_foreach(field_value) as elem_name:
                        elem_expr = f'{elem_to_any}({elem_name})'
                        elem_expr = self.move_as_any(elem_expr, move_expr=False)
                        self.gen_call(f'{array_name}.push_back', elem_expr)
                    field_expr = array_name
                case _:
                    raise ValueError(f'Unsupported property kind ({field_type["kind"]}): {field_type}')
            self.gen_call(
                f'{object_name}.emplace',
                f'"{field_name}"',
                self.move_as_any(field_expr, move_expr=False)
            )

    @gensym_context
    def generate_as_outgoing_params(
        self,
        params_name: str,
        message_symbol: LspSymbol
    ) -> None:
        message_spec = message_symbol.spec
        params_spec = message_spec.get("params", None)
        if params_spec is not None:
            params_type = params_spec["name"]
            with self.gen_fn(
                    'LspTransformer::asMessageParams',
                    'MessageParams',
                    params=[
                        f'const {params_type} &{params_name}',
                    ],
                    specs='const'
            ):
                outgoing_name = self.gensym_decl('MessageParams', 'messageParams')
                match params_spec["kind"]:
                    case "reference":
                        type_name = params_spec["name"]
                        match type_name:
                            case "LSPAny":
                                with self.gen_switch(f'{params_name}.type()'):
                                    with self.gen_case('LSPAnyType', 'object'):
                                        object_field = rename_field("LSPObject")
                                        object_name = self.gensym_ref('object', f'{params_name}.{object_field}()')
                                        self.gen_assign(f'{outgoing_name}', f'copy({object_name})')
                                        self.gen_break()
                                    with self.gen_case('LSPAnyType', 'array'):
                                        array_field = rename_field("LSPArray")
                                        array_name = self.gensym_ref('array', f'{params_name}.{array_field}()')
                                        self.gen_assign(f'{outgoing_name}', f'copy({array_name})')
                                        self.gen_break()
                                    with self.gen_default():
                                        self.gen_throw_invalid_params(
                                            f'("Invalid LSPAny type for {params_name}: LSPAnyType::" +',
                                            f' LSPAnyTypeNames.at({params_name}.type()))'
                                        )
                                self.newline()
                            case _:
                                symbol = self.resolve(type_name)
                                object_name = self.gensym_decl('LSPObject', 'object')
                                self.newline()
                                for field_type_name, field_spec in self.expand_fields(symbol.spec):
                                    field_name = field_spec['name']
                                    with self.nested_names_as(deque([field_type_name, field_name])):
                                        is_optional = field_spec.get('optional', False)
                                        field_value = f'{params_name}.{field_name}'
                                        self.gen_struct_as_outgoing_params(
                                            object_name,
                                            field_name,
                                            field_spec,
                                            field_value,
                                            is_optional
                                        )
                                        self.newline()
                                self.gen_assign(
                                    f'{outgoing_name}',
                                    f'std::move({object_name})'
                                )
                    case _:
                        raise ValueError(
                            f'Unsupported parameter type ({params_spec["kind"]}): {params_spec}'
                        )
                self.gen_return(outgoing_name)
            self.newline()

    def visit_request_symbol(self, request_symbol: LspSymbol) -> None:
        request_spec = request_symbol.spec
        match request_spec["messageDirection"]:
            case "clientToServer":
                self.generate_as_incoming_params("requestParams", request_symbol)
            case "serverToClient":
                self.generate_as_outgoing_params("requestParams", request_symbol)
            case "both":
                self.generate_as_incoming_params("requestParams", request_symbol)
                self.generate_as_outgoing_params("requestParams", request_symbol)
            case _:
                raise ValueError(
                    f'Unsupported messageDirection: {request_spec["messageDirection"]}'
                )

    def visit_notification_symbol(self, notification_symbol: LspSymbol) -> None:
        notification_spec = notification_symbol.spec
        match notification_spec["messageDirection"]:
            case "clientToServer":
                self.generate_as_incoming_params("notificationParams", notification_symbol)
            case "serverToClient":
                self.generate_as_outgoing_params("notificationParams", notification_symbol)
            case "both":
                self.generate_as_incoming_params("notificationParams", notification_symbol)
                self.generate_as_outgoing_params("notificationParams", notification_symbol)
            case _:
                raise ValueError(
                    f'Unsupported messageDirection: {notification_spec["messageDirection"]}'
                )

    def generate_code(self) -> None:
        print(f'Generating: {self.file_path}')
        self.generate_disclaimer()
        self.gen_include('stdexcept')
        self.newline()
        self.gen_include('server/lsp_exception.h')
        self.gen_include('server/lsp_specification.h')
        self.gen_include('server/lsp_transformer.h')
        self.newline()
        with self.gen_namespace(self.namespace):
            self.newline()
            super().generate_code()
            self.generate_copy_methods()
