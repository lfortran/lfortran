import sys
import traceback
from collections import defaultdict
from contextlib import contextmanager
from functools import wraps
from pathlib import Path
from typing import Callable, Dict, Iterator, List, Optional, Tuple, Union

from llanguage_server.file_generator import FileGenerator
from llanguage_server.lsp.datatypes import LspSpec, LspSymbol, LspSymbolKind
from llanguage_server.lsp.utils import (as_enumeration_spec,
                                        method_to_underscore, rename_enum,
                                        rename_field, rename_type, upper_first)
from llanguage_server.lsp.visitors import LspAnalysisPipeline


def gensym_context(fn: Callable) -> Callable:
    @wraps(fn)
    def wrapper(self, *args, **kwargs):
        old_serial_id_by_sym = self.serial_id_by_sym
        self.serial_id_by_sym = defaultdict(int)
        try:
            return fn(self, *args, **kwargs)
        finally:
            self.serial_id_by_sym = old_serial_id_by_sym
    return wrapper


class CPlusPlusLspFileGenerator(FileGenerator):
    schema: LspSpec
    pipeline: LspAnalysisPipeline
    namespace: str

    serial_id_by_sym: Dict[str, int] = defaultdict(int)

    _type_declaration_wrap_ptr: bool = False
    _type_declaration_wrap_nested_ptr: bool = False
    _type_declaration_string_ptr: bool = False
    _type_declaration_nested_string_ptr: bool = False

    def __init__(
            self,
            output_path: Path,
            schema: LspSpec,
            pipeline: LspAnalysisPipeline,
            namespace: str
    ) -> None:
        super().__init__(output_path)
        self.schema = schema
        self.pipeline = pipeline
        self.namespace = namespace

    def gensym(self, prefix: str = "tmp") -> str:
        next_id = self.serial_id_by_sym[prefix]
        self.serial_id_by_sym[prefix] += 1
        return f'{prefix}_{next_id}'

    def gensym_decl(self, kind_or_spec: Union[str, LspSpec], nym: str) -> str:
        kind: str
        match kind_or_spec:
            case str():
                kind = kind_or_spec
            case dict():
                spec: LspSpec = kind_or_spec
                kind = self.get_type_declaration(spec)
        sym = self.gensym(nym)
        self.write(f'{kind} {sym};')
        return sym

    def gensym_ref(self, nym: str, expr: str, const: bool = True) -> str:
        sym = self.gensym(nym)
        if const:
            self.gen_assign(f'const auto &{sym}', expr)
        else:
            self.gen_assign(f'auto &{sym}', expr)
        return sym

    def gensym_init(self, nym: str, expr: str) -> str:
        sym = self.gensym(nym)
        self.gen_assign(f'auto {sym}', expr)
        return sym

    def gensym_array(
            self,
            array_type_or_spec: Union[str, LspSpec],
            prefix: str,
            size_expr: Optional[str] = None
    ) -> str:
        array_type: str
        match array_type_or_spec:
            case str():
                array_type = array_type_or_spec
            case dict():
                array_spec: LspSpec = array_type_or_spec
                array_type = self.get_type_declaration(array_spec)
        name = self.gensym(prefix)
        self.write(f'{array_type} {name};')
        if size_expr is not None:
            self.write(f'{name}.reserve({size_expr});')
        return name

    def type_name_or_spec_to_decl(
            self,
            type_name_or_spec: Union[str, LspSpec]
    ) -> str:
        match type_name_or_spec:
            case str():
                return type_name_or_spec
            case dict():
                type_spec: LspSpec = type_name_or_spec
                return self.get_type_declaration(type_spec)
            case _:
                raise ValueError(
                    'Unsupported type ({kind}): {spec}'.format(
                        kind=type(type_name_or_spec),
                        spec=type_name_or_spec,
                    )
                )

    @contextmanager
    def gensym_foreach(
            self,
            coll_name: str,
            elem_prefix: str = 'elem'
    ) -> Iterator[str]:
        elem_name = self.gensym(elem_prefix)
        with self.gen_foreach(f'const auto &{elem_name}', coll_name):
            yield elem_name

    @contextmanager
    def gensym_foreach_keyval(
            self,
            map_name: str,
            key_prefix: str = 'key',
            value_prefix: str = 'value',
            const: bool = True
    ) -> Iterator[Tuple[str, str]]:
        key_name = self.gensym(key_prefix)
        value_name = self.gensym(value_prefix)
        prefix = 'const ' if const else ''
        with self.gen_foreach(f'{prefix}auto &[{key_name}, {value_name}]', map_name):
            yield key_name, value_name

    @contextmanager
    def block(
            self,
            indent: bool = True,
            end: str = '\n'
    ) -> Iterator:
        self.inline('{', indent=indent, end='\n')
        with self.indent():
            yield self
        self.inline('}', indent=True, end=end)

    def gen_include(self, header: str) -> None:
        self.write(f'#include <{header}>')

    def pragma_once(self) -> None:
        self.write('#pragma once')

    @contextmanager
    def gen_namespace(
            self,
            namespace: str
    ) -> Iterator:
        self.write(f'namespace {namespace} {{')
        with self.indent():
            yield self
        self.write(f'}} // namespace {namespace}')

    @contextmanager
    def gen_class(
            self,
            class_name: str,
            sups: Optional[List[Union[LspSpec, str]]] = None
    ) -> Iterator:
        if sups is None:
            self.write(f'class {class_name} {{')
        else:
            sups = [
                sup_spec["name"] if isinstance(sup_spec, dict) else sup_spec
                for sup_spec in sups
            ]
            self.write(f'class {class_name}')
            with self.indent():
                sup_name = sups[0]
                self.write(f': public {sup_name}')
                for i in range(1, len(sups)):
                    sup_name = sups[i]
                    self.write(f', public {sup_name}')
            self.write('{')
        yield self
        self.write(f'}}; // class {class_name}')

    @contextmanager
    def gen_public(self) -> Iterator:
        self.write('public:')
        with self.indent():
            yield self

    @contextmanager
    def gen_protected(self) -> Iterator:
        self.write('protected:')
        with self.indent():
            yield self

    @contextmanager
    def gen_private(self) -> Iterator:
        self.write('private:')
        with self.indent():
            yield self

    @contextmanager
    def gen_struct(
            self,
            struct_name: str,
            sups: Optional[List[LspSpec]] = None
    ) -> Iterator:
        if sups is None:
            self.write(f'struct {struct_name} {{')
        else:
            self.write(f'struct {struct_name}')
            with self.indent():
                sup_spec = sups[0]
                self.write(f': public {sup_spec["name"]}')
                for i in range(1, len(sups)):
                    sup_spec = sups[i]
                    self.write(f', public {sup_spec["name"]}')
            self.write('{')
        with self.indent():
            yield self
        self.write(f'}}; // struct {struct_name}')

    @contextmanager
    def gen_union(self) -> Iterator:
        self.write(f'union {{')
        with self.indent():
            yield self
        self.write('};')

    def gen_union_with(self, item_specs: List[LspSpec]) -> None:
        with self.gen_union():
            for item_index, item_spec in enumerate(item_specs):
                with self.nest_name(str(item_index)):
                    self.gen_union_field(item_spec)

    def gen_union_field(self, item_spec: LspSpec) -> None:
        with self.type_declaration_options(string_ptr=True):
            field_name = self.name_field(item_spec)
            field_type = self.get_type_declaration(item_spec)
            self.write(f'{field_type} _{field_name};')

    @contextmanager
    def gen_enum(self, name: str) -> Iterator:
        self.write(f'enum class {name} {{')
        with self.indent():
            yield self
        self.write(f'}}; // enum class {name}')

    def gen_enum_with(self, enum_name: str, item_specs: List[LspSpec]) -> None:
        with self.gen_enum(enum_name):
            self.write('UNINITIALIZED = -1,')
            for index, item_spec in enumerate(item_specs):
                with self.nest_name(str(index)):
                    self.write(f'{self.get_union_enumeration(item_spec)} = {index},')

    def gen_union_tag_field(self, enum_name: str) -> None:
        line = f'{self.indentation()}{enum_name} _type{{{enum_name}::UNINITIALIZED}};'
        if len(line) < self.column_width:
            self.inline(line, end='\n')
        else:
            self.write(f'{enum_name} _type{{')
            with self.indent():
                self.write(f'{enum_name}::UNINITIALIZED')
            self.write('};')

    @contextmanager
    def gen_switch(self, value: str) -> Iterator:
        self.write(f'switch ({value}) {{')
        yield self
        self.write('}')

    def gen_map_decl(self, map_name: str, key_type: str, val_type: str) -> None:
        line = f'{self.indentation()}extern std::map<{key_type}, {val_type}> {map_name};'
        if len(line) < self.column_width:
            self.inline(line, end='\n')
        else:
            line = f'{self.indentation()}extern std::map<{key_type}, {val_type}>'
            if len(line) < self.column_width:
                self.inline(line, end='\n')
                with self.indent():
                    self.write(f'{map_name};')
            else:
                self.write('extern std::map<')
                with self.indent():
                    self.write(f'{key_type},')
                    self.write(val_type)
                self.write(f'> {map_name};')

    @contextmanager
    def gen_map(
            self,
            map_name: str,
            key_type: str,
            val_type: str
    ) -> Iterator:
        line = f'{self.indentation()}std::map<{key_type}, {val_type}> {map_name} = {{'
        if len(line) < self.column_width:
            self.inline(line, end='\n')
        else:
            line = f'{self.indentation()}std::map<{key_type}, {val_type}>'
            if len(line) < self.column_width:
                self.inline(line, end='\n')
                self.write(f'{map_name} = {{')
            else:
                self.write('std::map<')
                with self.indent():
                    self.write(f'{key_type},')
                    self.write(val_type)
                self.write(f'> {map_name} = {{')
        with self.indent():
            yield self
        self.write('};')

    def gen_map_entry(self, key: str, value: str) -> None:
        line = f'{self.indentation()}{{{key}, {value}}},'
        if len(line) < self.column_width:
            self.inline(line, end='\n')
        else:
            self.write(f'{{{key},')
            self.write(f' {value}}},')

    def gen_enum_map_entry(
            self,
            enumeration: str,
            enumerator_or_spec: Union[str, LspSpec],
            value_or_spec: Union[str, LspSpec]
    ) -> None:
        match (enumerator_or_spec, value_or_spec):
            case (str(), str()):
                enumerator: str = enumerator_or_spec
                key = f'{enumeration}::{enumerator}'
                value = f'"{value_or_spec}"'
            case (str(), dict()):
                enumerator: str = enumerator_or_spec
                value_spec: LspSpec = value_or_spec
                key = f'{enumeration}::{enumerator}'
                value = '"{enum_value}"'.format(
                    enum_value=self.get_union_enumeration(value_spec)
                )
            case (dict(), str()):
                enumerator_spec: LspSpec = enumerator_or_spec
                key = f'{enumeration}::{{enumerator}}'.format(
                    enumerator=self.get_union_enumeration(enumerator_spec)
                )
                value = f'"{value_or_spec}"'
            case (dict(), dict()):
                enumerator_spec = enumerator_or_spec
                value_spec = value_or_spec
                key = f'{enumeration}::{{enumerator}}'.format(
                    enumerator=self.get_union_enumeration(enumerator_spec)
                )
                value = '"{enum_value}"'.format(
                    enum_value=self.get_union_enumeration(value_spec)
                )
            case _:
                raise ValueError(
                    f'Unsupported enumerator type ({type(enumerator_or_spec)}) or ' +
                    f'value type ({type(value_or_spec)})'
                )
        self.gen_map_entry(key, value)

    @contextmanager
    def gen_case(
            self,
            enum_name: str,
            spec_or_name: Union[LspSpec, str]
    ) -> Iterator:
        enumerator = self.get_union_enumeration(spec_or_name)
        self.inline(f'case {enum_name}::{rename_enum(enumerator)}: ', indent=True)
        with self.block(indent=False):
            yield self

    @contextmanager
    def gen_default(self) -> Iterator:
        self.inline('default: ', indent=True)
        with self.block(indent=False):
            yield self

    @contextmanager
    def gen_try(self) -> Iterator:
        self.write('try {')
        with self.indent():
            yield self
        self.inline('}', indent=True, end='')

    @contextmanager
    def gen_catch(self, expr: str, end: str = '\n') -> Iterator:
        self.inline(f' catch ({expr}) {{', end='\n')
        with self.indent():
            yield self
        self.inline('}', indent=True, end=end)

    @contextmanager
    def gen_if(self, expr: str, end: str = '\n') -> Iterator:
        self.write(f'if ({expr}) {{')
        with self.indent():
            yield self
        self.inline('}', indent=True, end=end)

    @contextmanager
    def gen_if_eq(self, lhs: str, rhs: str, end: str = '\n') -> Iterator:
        line = f'{self.indentation()}if ({lhs} == {rhs}) {{'
        if len(line) < self.column_width:
            self.inline(line, end='\n')
        else:
            self.write(f'if ({lhs} ==')
            with self.indent(2):
                self.write(f'{rhs}) {{')
        with self.indent():
            yield self
        self.inline('}', indent=True, end=end)

    @contextmanager
    def gen_if_ne(self, lhs: str, rhs: str, end: str = '\n') -> Iterator:
        line = f'{self.indentation()}if ({lhs} != {rhs}) {{'
        if len(line) < self.column_width:
            self.inline(line, end='\n')
        else:
            self.write(f'if ({lhs} !=')
            with self.indent(2):
                self.write(f'{rhs}) {{')
        with self.indent():
            yield self
        self.inline('}', indent=True, end=end)

    @contextmanager
    def gen_elif(self, expr: str, end: str = '\n') -> Iterator:
        self.inline(f' else if ({expr}) {{', end='\n')
        with self.indent():
            yield self
        self.inline('}', indent=True, end=end)

    @contextmanager
    def gen_else(self) -> Iterator:
        self.inline(' else {', end='\n')
        with self.indent():
            yield self
        self.write('}')

    @contextmanager
    def gen_for(self, init: str, pred: str, step: str) -> Iterator:
        line = f'{self.indentation()}for ({init}; {pred}; {step}) {{'
        if len(line) < self.column_width:
            self.inline(line, end='\n')
        else:
            self.write(f'for (')
            with self.indent():
                self.write(f'{init};')
                self.write(f'{pred};')
                self.write(f'{step}')
            self.write(') {')
        with self.indent():
            yield self
        self.write('}')

    @contextmanager
    def gen_foreach(self, elem: str, coll: str) -> Iterator:
        line = f'{self.indentation()}for ({elem} : {coll}) {{'
        if len(line) < self.column_width:
            self.inline(line, end='\n')
        else:
            self.write(f'for ({elem}')
            with self.indent(2):
                self.write(f': {coll}) {{')
        with self.indent():
            yield self
        self.write('}')

    @contextmanager
    def gen_while(self, pred: str) -> Iterator:
        self.write(f'while ({pred}) {{')
        with self.indent():
            yield self
        self.write('}')

    @contextmanager
    def gen_do(self, pred: str) -> Iterator:
        self.write('do {')
        with self.indent():
            yield self
        self.write(f'}} while ({pred});')

    def gen_assign(self, lhs: str, rhs: str) -> None:
        line = f'{self.indentation()}{lhs} = {rhs};'
        if len(line) < self.column_width:
            self.inline(line, end='\n')
        else:
            self.write(f'{lhs} =')
            with self.indent():
                self.write(f'{rhs};')

    def gen_assign_enum(
            self,
            lhs: str,
            enumeration: str,
            enumerator: Union[str, LspSpec]
    ) -> None:
        enumerator = self.get_union_enumeration(enumerator)
        self.gen_assign(lhs, f'{enumeration}::{enumerator}')

    def gen_fn_decl(
            self,
            fn: str,
            ret_type: str = 'void',
            params: List[Union[str, LspSpec]] = [],
            specs: Optional[Union[str, List[str]]] = None,
            docs: Optional[str] = None,
            virtual: bool = False,
            override: bool = False
    ) -> None:
        self.generate_docstring(docs)
        params = [(self.get_type_declaration(param)
                   if isinstance(param, dict)
                   else param)
                  for param in params]
        prefix = 'virtual ' if virtual else ''
        suffix = ' override' if override else ''
        line = f'{self.indentation()}{prefix}auto {fn}({", ".join(params)})'
        if specs is not None:
            if isinstance(specs, list):
                specs = " ".join(specs)
            line += f' {specs}'
        line += f' -> {ret_type}{suffix};'
        if len(line) < self.column_width:
            self.inline(line, end='\n')
        else:
            if len(params) == 0:
                self.inline(f'{prefix}auto {fn}()', indent=True)
                if specs is not None:
                    self.inline(f' {specs}')
                self.inline(end='\n')
                self.inline(indent=True)
            else:
                self.write(f'{prefix}auto {fn}(')
                with self.indent():
                    self.inline(params[0], indent=True)
                    for i in range(1, len(params)):
                        self.inline(',', end='\n')
                        self.inline(params[i], indent=True)
                    self.inline(end='\n')
                self.inline(') ', indent=True)
                if specs is not None:
                    self.inline(f'{specs} ')
            self.inline(f'-> {ret_type}{suffix};', end='\n')

    @contextmanager
    def gen_fn(
            self,
            fn: str,
            ret_type_or_spec: Union[str, LspSpec] = 'void',
            params: List[Union[str, LspSpec]] = [],
            specs: Optional[str] = None,
            inline: bool = False
    ) -> Iterator:
        ret_type: str
        match ret_type_or_spec:
            case str():
                ret_type = ret_type_or_spec
            case dict():
                type_spec: LspSpec = ret_type_or_spec
                ret_type = self.get_type_declaration(type_spec)
        params = [(self.get_type_declaration(param)
                   if isinstance(param, dict)
                   else param)
                  for param in params]
        if inline:
            prefix = 'inline '
        else:
            prefix = ''
        line = f'{self.indentation()}{prefix}auto {fn}({", ".join(params)})'
        if specs is not None:
            if isinstance(specs, list):
                specs = " ".join(specs)
            line += f' {specs}'
        line += f' -> {ret_type}'
        if len(line) < self.column_width:
            self.inline(line, end='\n')
        else:
            if len(params) == 0:
                self.inline(f'{prefix}auto {fn}()', indent=True)
                if specs is not None:
                    self.inline(f' {specs}')
                self.inline(end='\n')
                self.inline(indent=True)
            else:
                self.write(f'{prefix}auto {fn}(')
                with self.indent():
                    self.inline(params[0], indent=True)
                    for i in range(1, len(params)):
                        self.inline(',', end='\n')
                        self.inline(params[i], indent=True)
                    self.inline(end='\n')
                self.inline(') ', indent=True)
                if specs is not None:
                    self.inline(f'{specs} ')
            self.inline(f'-> {ret_type}', end='\n')
        with self.block():
            yield self

    def gen_constructor_decl(
            self,
            kind: str,
            params: List[Union[str, LspSpec]] = [],
            specs: Optional[str] = None
    ) -> None:
        params = [(self.get_type_declaration(param)
                   if isinstance(param, dict)
                   else param)
                  for param in params]
        line = f'{self.indentation()}{kind}({", ".join(params)})'
        if specs is not None:
            if isinstance(specs, list):
                specs = " ".join(specs)
            line += f' {specs}'
        line += ";"
        if len(line) < self.column_width or len(params) == 0:
            self.inline(line, end='\n')
        else:
            self.write(f'{kind}(')
            with self.indent():
                if len(params) > 0:
                    self.inline(params[0], indent=True)
                    for i in range(1, len(params)):
                        self.inline(',', end='\n')
                        self.inline(params[i], indent=True)
                    self.inline(end='\n')
                else:
                    self.write('// empty')
            self.inline(')', indent=True)
            if specs is not None:
                self.inline(f' {specs}')
            self.inline(';', end='\n')

    def gen_copy_constructor_decl(
            self,
            kind: str,
            specs: Optional[str] = None
    ) -> None:
        self.gen_constructor_decl(
            kind,
            params=[
                f'const {kind} &other',
            ],
            specs=specs
        )

    def gen_move_constructor_decl(
            self,
            kind: str,
            specs: Optional[str] = None
    ) -> None:
        self.gen_constructor_decl(
            kind,
            params=[
                f'{kind} &&other',
            ],
            specs=specs
        )

    def get_sup_name_and_params(
            self,
            sup_spec_opt_params: Union[
                LspSpec,
                Tuple[
                    Union[LspSpec, str],
                    List[str]
                ]
            ]
    ) -> Tuple[str, str]:
        match sup_spec_opt_params:
            case dict():
                sup_spec = sup_spec_opt_params
                sup_name = sup_spec["name"]
                params = ""
            case tuple() | list():
                sup_spec_or_name, params = sup_spec_opt_params
                match sup_spec_or_name:
                    case dict():
                        sup_spec = sup_spec_or_name
                        sup_name = sup_spec["name"]
                    case str():
                        sup_name = sup_spec_or_name
                params = ", ".join(params)
        return sup_name, params

    @contextmanager
    def gen_constructor(
            self,
            kind: str,
            params: List[Union[str, LspSpec]] = [],
            sups: Optional[
                List[
                    Union[
                        LspSpec,
                        Tuple[
                            Union[LspSpec, str],
                            List[str]
                        ]
                    ]
                ]
            ] = None,
            inits: Optional[
                List[
                    Tuple[str, List[str]]
                ]
            ] = None,
            specs: Optional[str] = None,
    ) -> Iterator:
        params = [(self.get_type_declaration(param)
                   if isinstance(param, dict)
                   else param)
                  for param in params]
        line = f'{self.indentation()}{kind}::{kind}({", ".join(params)})'
        if specs is not None:
            if isinstance(specs, list):
                specs = " ".join(specs)
            line += f' {specs}'
        if len(line) < self.column_width or len(params) == 0:
            self.inline(line, end='\n')
        else:
            self.write(f'{kind}::{kind}(')
            with self.indent():
                if len(params) > 0:
                    self.inline(params[0], indent=True)
                    for i in range(1, len(params)):
                        self.inline(',', end='\n')
                        self.inline(params[i], indent=True)
                    self.inline(end='\n')
                else:
                    self.write('// empty')
            self.inline(')', indent=True)
            if specs is not None:
                self.inline(f' {specs}')
            self.inline(end='\n')
        if sups is not None:
            with self.indent():
                sup_name, params = self.get_sup_name_and_params(sups[0])
                self.write(f': {sup_name}({params})')
                for i in range(1, len(sups)):
                    sup_name, params = self.get_sup_name_and_params(sups[i])
                    self.write(f', {sup_name}({params})')
        if inits is not None:
            with self.indent():
                init_name, init_params = inits[0]
                if sups is not None:
                    self.write(f', {init_name}({", ".join(init_params)})')
                else:
                    self.write(f': {init_name}({", ".join(init_params)})')
                for i in range(1, len(inits)):
                    init_name, init_params = inits[i]
                    self.write(f', {init_name}({", ".join(init_params)})')
        with self.block():
            yield self

    def gen_destructor_decl(
            self,
            kind: str,
            params: List[Union[str, LspSpec]] = [],
            specs: Optional[str] = None
    ) -> None:
        params = [(self.get_type_declaration(param)
                   if isinstance(param, dict)
                   else param)
                  for param in params]
        line = f'{self.indentation()}~{kind}({", ".join(params)})'
        if specs is not None:
            if isinstance(specs, list):
                specs = " ".join(specs)
            line += f' {specs}'
        line += ";"
        if len(line) < self.column_width or len(params) == 0:
            self.inline(line, end='\n')
        else:
            self.write(f'~{kind}(')
            with self.indent():
                self.inline(params[0], indent=True)
                for i in range(1, len(params)):
                    self.inline(',', end='\n')
                    self.inline(params[i], indent=True)
                self.inline(end='\n')
            self.inline(')', indent=True)
            if specs is not None:
                self.inline(f' {specs}')
            self.inline(';', end='\n')

    @contextmanager
    def gen_destructor(
            self,
            kind: str,
            params: List[Union[str, LspSpec]] = [],
            specs: Optional[str] = None
    ) -> Iterator:
        params = [(self.get_type_declaration(param)
                   if isinstance(param, dict)
                   else param)
                  for param in params]
        line = f'{self.indentation()}{kind}::~{kind}({", ".join(params)})'
        if specs is not None:
            if isinstance(specs, list):
                specs = " ".join(specs)
            line += f' {specs}'
        if len(line) < self.column_width or len(params) == 0:
            self.inline(line, end='\n')
        else:
            self.write(f'{kind}::~{kind}(')
            with self.indent():
                self.inline(params[0], indent=True)
                for i in range(1, len(params)):
                    self.inline(',', end='\n')
                    self.inline(params[i], indent=True)
                self.inline(end='\n')
            self.inline(')', indent=True)
            if specs is not None:
                self.inline(f' {specs}')
            self.inline(end='\n')
        with self.block():
            yield self

    def gen_call(self, fn: str, *args: str) -> None:
        line = f'{self.indentation()}{fn}({", ".join(args)});'
        if len(line) < self.column_width or len(args) == 0:
            self.inline(line, end='\n')
        else:
            self.write(f'{fn}(')
            with self.indent():
                self.inline(args[0], indent=True)
                for i in range(1, len(args)):
                    self.inline(',', end='\n')
                    self.inline(args[i], indent=True)
                self.inline(end='\n')
            self.write(');')

    def gen_throw(self, exc: str, *args: str) -> None:
        self.gen_call(f'throw {exc}', *args)

    def resolve(self, name_or_spec_or_symbol: Union[str, LspSpec, LspSymbol]) -> LspSymbol:
        name: str
        symbol: Optional[LspSymbol]
        match name_or_spec_or_symbol:
            case str():
                name = name_or_spec_or_symbol
                symbol = self.pipeline.indexer.name_index.get(name, None)
            case dict():
                spec: LspSpec = name_or_spec_or_symbol
                match spec["kind"]:
                    case "base" | "reference":
                        name = spec["name"]
                    case "or" | "literal" | "array" | "map" | "tuple":
                        name = self.nested_name()
                    case _:
                        raise ValueError(f'Unsupported symbol type ({spec["kind"]}): {spec}')
                symbol = self.pipeline.indexer.name_index.get(name, None)
            case LspSymbol():
                symbol = name_or_spec_or_symbol
            case _:
                raise ValueError(
                    f'Unsupported symbol type ({type(name_or_spec_or_symbol)}): {name_or_spec_or_symbol}'
                )
        if symbol is not None and symbol.resolution is not None:
            return symbol.resolution
        raise ValueError(f'Failed to resolve symbol: {name_or_spec_or_symbol}')

    def generate_disclaimer(self) -> None:
        self.write('// ------------------------------------------------------------------------------')
        self.write('// NOTE: This file was generated from Microsoft\'s Language Server Protocol (LSP)')
        self.write('// specification. Please do not edit it by hand.')
        self.write('// ------------------------------------------------------------------------------')
        self.newline()

    def generate_docstring(self, docs: Optional[str]) -> None:
        if docs is not None:
            try:
                docs = docs.replace(r'​', '') \
                           .replace(r'*/', '*{@literal /}') \
                           .replace(r'/*', '{@literal /}*')
                self.write( '/**')
                for line in docs.split('\n'):
                    self.inline(' *', indent=True)
                    if len(line) > 0:
                        self.inline(f' {line}')
                    self.newline()
                self.write( ' */')
            except Exception as e:
                stack_trace = traceback.format_exc()
                self.write('/*')
                self.write(stack_trace)
                self.write('/* DEBUG: Recovered from error, here. */')
                print(f"Recovered from failure while generating documentation.", file=sys.stderr)
                print(stack_trace, file=sys.stderr)

    def get_union_enumeration(
        self,
        spec_or_name: Union[str, LspSpec]
    ) -> str:
        match spec_or_name:
            case str():
                name: str = spec_or_name
                return rename_enum(name)
            case dict():
                spec: LspSpec = spec_or_name
                match spec["kind"]:
                    case "base" | "reference":
                        return rename_enum(spec["name"])
                    case "stringLiteral":
                        return rename_enum("string")
                    case "integerLiteral":
                        return rename_enum("integer")
                    case "booleanLiteral":
                        return rename_enum("boolean")
                    case "array":
                        return f'{self.get_union_enumeration(spec["element"])}_ARRAY'
                    case "map":
                        key = self.get_union_enumeration(spec["key"])
                        val = self.get_union_enumeration(spec["value"])
                        return f'{key}{val}'
                    case "and":
                        raise ValueError(
                            f'AND types are not supported for type declarations: {spec}'
                        )
                    case "or":
                        buffer = []
                        for index, item_spec in enumerate(spec["items"]):
                            with self.nest_name(str(index)):
                                if index > 0:
                                    buffer.append('_OR_')
                                buffer.append(self.get_union_enumeration(item_spec))
                        return "".join(buffer)
                    case "tuple":
                        buffer = []
                        item_specs = spec["items"]
                        if len(item_specs) == 2:
                            buffer.append('PAIR_OF_')
                        else:
                            buffer.append('TUPLE_OF_')
                        for index, item_spec in enumerate(item_specs):
                            if index > 0:
                                buffer.append('_AND_')
                            buffer.append(self.get_union_enumeration(item_spec))
                        return "".join(buffer)
                    case "literal":
                        return rename_enum(self.nested_name())
                    case _:
                        raise ValueError(f'Unsupported union enumeration kind ({spec["kind"]}): {spec}')

    def generate_union_enumeration(
        self,
        spec: LspSpec
    ) -> None:
        self.inline(self.get_union_enumeration(spec))

    def get_enum_value(self, enumeration: str, enumerator: Union[str, LspSpec]) -> str:
        match enumerator:
            case str():
                return f'{enumeration}::{enumerator}'
            case dict():
                enumerator = self.get_union_enumeration(enumerator)
                return self.get_enum_value(enumeration, enumerator)
            case _:
                raise ValueError(f'Unsupported enumerator type ({type(enumerator)}): {enumerator}')

    def name_field(self, type_spec: LspSpec) -> str:
        match type_spec["kind"]:
            case "base" | "reference":
                field_name = rename_field(type_spec["name"])
            case "literal":
                field_name = self.nested_name()
                field_name = rename_field(field_name)
            case "tuple":
                elem_specs = type_spec["items"]
                if len(elem_specs) == 2:
                    field_name = "pairOf"
                else:
                    field_name = "tupleOf"
                for index, elem_spec in enumerate(elem_specs):
                    if index > 0:
                        field_name += "And"
                    match elem_spec["kind"]:
                        case "base" | "reference":
                            field_name += upper_first(elem_spec["name"])
                        case _:
                            raise ValueError(f'Unsupported tuple field type ({elem_spec["kind"]}): {elem_spec}')
            case "array":
                elem_spec = type_spec["element"]
                match elem_spec["kind"]:
                    case "base" | "reference":
                        elem_type_name = elem_spec["name"]
                        elem_symbol = self.resolve(elem_type_name)
                        field_name = elem_symbol.normalized_name
                        field_name = f'{field_name}Array'
                        field_name = rename_field(field_name)
                    case "or":
                        field_name = self.nested_name()
                        field_name = rename_field(field_name)
                    case _:
                        raise ValueError(f'Unsupported array field type ({elem_spec["kind"]}): {elem_spec}')
            case _:
                raise ValueError(f'Unsupported field type ({type_spec["kind"]}): {type_spec}')
        return field_name

    @contextmanager
    def type_declaration_options(
            self,
            wrap_ptr: Optional[bool] = None,
            wrap_nested_ptr: Optional[bool] = None,
            string_ptr: Optional[bool] = None,
            nested_string_ptr: Optional[bool] = None
    ) -> Iterator:
        if wrap_ptr is None:
            wrap_ptr = self._type_declaration_wrap_ptr
        if wrap_nested_ptr is None:
            wrap_nested_ptr = self._type_declaration_wrap_nested_ptr
        if string_ptr is None:
            string_ptr = self._type_declaration_string_ptr
        if nested_string_ptr is None:
            nested_string_ptr = self._type_declaration_nested_string_ptr

        old_wrap_ptr = self._type_declaration_wrap_ptr
        old_wrap_nested_ptr = self._type_declaration_wrap_nested_ptr
        old_string_ptr = self._type_declaration_string_ptr
        old_nested_string_ptr = self._type_declaration_nested_string_ptr

        self._type_declaration_wrap_ptr = wrap_ptr
        self._type_declaration_wrap_nested_ptr = wrap_nested_ptr
        self._type_declaration_string_ptr = string_ptr
        self._type_declaration_nested_string_ptr = nested_string_ptr

        yield self

        self._type_declaration_wrap_ptr = old_wrap_ptr
        self._type_declaration_wrap_nested_ptr = old_wrap_nested_ptr
        self._type_declaration_string_ptr = old_string_ptr
        self._type_declaration_nested_string_ptr = old_nested_string_ptr

    def _wrap_ptr(self, level: int) -> bool:
        return self._type_declaration_wrap_ptr and level == 0 \
            or self._type_declaration_wrap_nested_ptr and level > 0

    def _string_ptr(self, level: int) -> bool:
        return self._type_declaration_string_ptr and level == 0 \
            or self._type_declaration_nested_string_ptr and level > 0

    def wrap_ptr(self, type_name: str, level: int) -> str:
        if self._wrap_ptr(level):
            type_name = f'std::unique_ptr<{type_name}>'
        return type_name

    def get_type_declaration(
            self,
            type_spec_or_name_or_symbol: Union[str, LspSpec, LspSymbol],
            level: int = 0) -> str:
        next_level = level + 1
        match type_spec_or_name_or_symbol:
            case dict():
                type_spec: LspSpec = type_spec_or_name_or_symbol
                match type_spec["kind"]:
                    case "base":
                        type_name = type_spec["name"]
                        return self.get_type_declaration(type_name, level=level)
                    case "reference":
                        type_name = type_spec["name"]
                        match type_name:
                            case "LSPAny" | "LSPObject" | "LSPArray":
                                return self.wrap_ptr(type_name, level)
                            case _:
                                symbol = self.resolve(type_name)
                                match symbol.kind:
                                    case LspSymbolKind.BASE:
                                        type_name = symbol.normalized_name
                                        return self.get_type_declaration(type_name, level=level)
                                    case LspSymbolKind.STRUCTURE | \
                                        LspSymbolKind.UNION | \
                                        LspSymbolKind.INNER | \
                                        LspSymbolKind.ARRAY | \
                                        LspSymbolKind.MAP | \
                                        LspSymbolKind.TUPLE:
                                        return self.wrap_ptr(type_name, level)
                                    case _:
                                        return type_name
                    case "array":
                        array_spec = type_spec
                        elem_spec = array_spec['element']
                        array_name = self.nested_name()
                        with self.type_declaration_options(
                                string_ptr=False,
                                wrap_ptr=self.has_cycle(array_name, elem_spec)
                        ):
                            with self.nest_name('elem'):
                                elem = self.get_type_declaration(elem_spec, level=0)
                        type_name = f'std::vector<{elem}>'
                        return self.wrap_ptr(type_name, level)
                    case "map":
                        map_spec = type_spec
                        key_spec = map_spec["key"]
                        value_spec = map_spec["value"]
                        map_name = self.nested_name()
                        with self.type_declaration_options(
                                string_ptr=False,
                                wrap_ptr=self.has_cycle(map_name, value_spec)
                        ):
                            with self.nest_name('key'):
                                key = self.get_type_declaration(key_spec, level=0)
                            with self.nest_name('value'):
                                val = self.get_type_declaration(value_spec, level=0)
                        type_name = f'std::map<{key}, {val}>'
                        return self.wrap_ptr(type_name, level)
                    case "and":
                        raise ValueError(
                            f'AND types are not supported for type declarations: {type_spec}'
                        )
                    case "or":
                        type_name = self.nested_name()
                        return self.wrap_ptr(type_name, level)
                    case "tuple":
                        with self.type_declaration_options(string_ptr=False):
                            item_specs: List[LspSpec] = type_spec["items"]
                            items = ", ".join((self.get_type_declaration(item_spec)
                                               for item_spec in item_specs))
                            items: Union[str, List[str]] = []
                            for item_index, item_spec in enumerate(item_specs):
                                with self.nest_name(str(item_index)):
                                    item = self.get_type_declaration(item_spec, level=next_level)
                                    items.append(item)
                            items = ", ".join(items)
                            if len(item_specs) == 2:
                                type_name = f'std::pair<{items}>'
                            else:
                                type_name = f'std::tuple<{items}>'
                            return self.wrap_ptr(type_name, level)
                    case "literal":
                        type_name = self.nested_name()
                        return self.wrap_ptr(type_name, level)
                    case "stringLiteral":
                        return rename_type('string')
                    case "integerLiteral":
                        return rename_type('integer')
                    case "booleanLiteral":
                        return rename_type('boolean')
                    case _:
                        raise ValueError(f'Unsupported Type kind: {type_spec}')
            case str():
                type_name: str = type_spec_or_name_or_symbol
                symbol = self.resolve(type_name)
                if symbol.normalized_name == "string" and self._string_ptr(level):
                    return f'std::unique_ptr<{symbol.resolved_name}>'
                return symbol.resolved_name
            case LspSymbol():
                symbol: LspSymbol = type_spec_or_name_or_symbol
                return self.get_type_declaration(symbol.spec)
            case _:
                raise ValueError(
                    f'Unsupported spec type ({type(type_spec_or_name)}): {type_spec_or_name}'
                )

    def is_union_pointer_type(self, field_spec: LspSpec) -> bool:
        match field_spec["kind"]:
            case "base":
                base_symbol = self.resolve(field_spec["name"])
                return base_symbol.normalized_name == "string"
            case "reference":
                symbol = self.resolve(field_spec["name"])
                match symbol.kind:
                    case LspSymbolKind.BASE:
                        return symbol.normalized_name == "string"
                    case LspSymbolKind.ENUMERATION:
                        return False
                    case LspSymbolKind.STRUCTURE \
                         | LspSymbolKind.ARRAY \
                         | LspSymbolKind.TUPLE \
                         | LspSymbolKind.MAP \
                         | LspSymbolKind.UNION:
                        return True
                    case _:
                        raise ValueError(
                            f'Unsupported field type ({symbol.kind}): {symbol.spec}'
                        )
            case "enumeration":
                return False
            case "array" | "map" | "tuple" | "or" | "literal":
                return True
            case _:
                raise ValueError(
                    f'Unsupported field type ({field_spec["kind"]}): {field_spec}'
                )

    def generate_type_declaration(self, type_spec: LspSpec) -> None:
        self.inline(self.get_type_declaration(type_spec))

    def rename_base_type(self, base_name: str) -> str:
        match base_name:
            case "boolean":
                return "bool"
            case "decimal":
                return "double"
            case "integer":
                return "int"
            case "uinteger":
                return "unsigned int"
            case "string" | "DocumentUri" | "RegExp" | "URI":
                return "std::string"
            case "null":
                return "std::nullptr_t"
            case _:
                raise ValueError(f'Unrecognized LSP base type name: {base_name}')

    def generate_enumeration(self, enum_spec: LspSpec) -> None:
        pass

    def generate_message_enum(self, enum_name: str, symbols: List[LspSymbol]) -> None:
        enumeration = []
        for symbol in symbols:
            message_method = symbol.name
            enumerator = method_to_underscore(message_method)
            enumeration.append((enumerator, message_method))
        enum_spec = as_enumeration_spec(enum_name, enumeration)
        self.generate_enumeration(enum_spec)

    def generate_request_enums(self) -> None:
        self.generate_message_enum(
            'IncomingRequest',
            self.pipeline.indexer.request_index["clientToServer"]
        )
        self.generate_message_enum(
            'OutgoingRequest',
            self.pipeline.indexer.request_index["serverToClient"]
        )

    def generate_notification_enums(self) -> None:
        self.generate_message_enum(
            'IncomingNotification',
            self.pipeline.indexer.notification_index["clientToServer"]
        )
        self.generate_message_enum(
            'OutgoingNotification',
            self.pipeline.indexer.notification_index["serverToClient"]
        )

    @contextmanager
    def gen_if_debug_enabled(self, if_expr: Optional[str] = None) -> Iterator:
        self.inline('#ifdef DEBUG', end='\n')
        if if_expr is not None:
            with self.gen_if(if_expr):
                yield self
        else:
            yield self
        self.inline('#endif // DEBUG', end='\n')

    @contextmanager
    def gen_throw_lsp_exception(self, exception_type: str) -> Iterator:
        self.write('throw LSP_EXCEPTION(')
        with self.indent():
            self.write(f'{exception_type},')
            yield self
        self.write(');')

    def gen_throw_invalid_params(self, *fragments: str) -> None:
        with self.gen_throw_lsp_exception('ErrorCodes::INVALID_PARAMS'):
            for fragment in fragments:
                self.write(fragment)

    def gen_return(self, expr: str) -> None:
        self.write(f'return {expr};')

    def gen_break(self) -> None:
        self.write('break;')

    def has_cycle(
            self,
            source_or_name: Union[LspSymbol, str],
            name_or_spec_or_symbol: Union[str, LspSpec, LspSymbol]
    ) -> bool:
        source: LspSymbol
        match source_or_name:
            case LspSymbol():
                source = source_or_name
            case str():
                name: str = source_or_name
                try:
                    source = self.resolve(name)
                except ValueError:
                    return False
        if source.cycles is None:
            return False
        name: str
        spec: LspSpec
        target: LspSymbol
        match name_or_spec_or_symbol:
            case str():
                name = name_or_spec_or_symbol
                target = self.resolve(name)
            case dict():
                spec = name_or_spec_or_symbol
                match spec["kind"]:
                    case "base" | "reference":
                        name = spec["name"]
                    case "or" | "literal" | "array" | "map" | "tuple":
                        name = self.nested_name()
                    case "stringLiteral" | "integerLiteral" | "booleanLiteral":
                        return False
                    case _:
                        raise NotImplementedError(
                            f'Unsupported spec type ({spec["kind"]}): {spec}'
                        )
                target = self.resolve(name)
            case LspSymbol():
                target = name_or_spec_or_symbol
        return target in source.cycles
