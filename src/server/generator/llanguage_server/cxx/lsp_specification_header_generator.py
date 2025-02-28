from functools import wraps
from pathlib import Path
from typing import Callable, Dict, Set, Tuple

from llanguage_server.cxx.visitors import BaseCPlusPlusLspVisitor
from llanguage_server.lsp.datatypes import LspSpec, LspSymbol, LspSymbolKind
from llanguage_server.lsp.utils import (lower_first, normalize_name, rename_enum,
                                        rename_type, upper_first)
from llanguage_server.lsp.visitors import LspAnalysisPipeline


class CPlusPlusSpecificationHeaderGenerator(BaseCPlusPlusLspVisitor):

    def __init__(
            self,
            output_dir: Path,
            schema: LspSpec,
            pipeline: LspAnalysisPipeline,
            namespace: str
    ) -> None:
        specification_header = output_dir / "lsp_specification.h"
        super().__init__(specification_header, schema, pipeline, namespace)

    def generate_struct_field(
            self,
            struct_symbol: LspSymbol,
            field_spec: LspSpec
    ) -> None:
        field_name: str = field_spec["name"]
        wrap_ptr = False
        wrap_nested_ptr = False
        match field_spec["type"]["kind"]:
            case "reference":
                dependency = self.resolve(field_spec["type"]["name"])
                wrap_ptr = self.has_cycle(struct_symbol, dependency)
            case "array":
                array_spec = field_spec["type"]
                elem_spec = array_spec["element"]
                wrap_nested_ptr = self.has_cycle(struct_symbol, elem_spec)
            case "map":
                map_spec = field_spec["type"]
                value_spec = map_spec["value"]
                wrap_nested_ptr = self.has_cycle(struct_symbol, value_spec)
            case _:
                pass
        with self.type_declaration_options(
                wrap_ptr=wrap_ptr,
                wrap_nested_ptr=wrap_nested_ptr
        ):
            field_type = self.get_type_declaration(field_spec["type"])
        with self.nest_name(field_name):
            if field_spec.get("optional", False):
                line = f'{self.indentation()}std::optional<{field_type}> {field_name};'
                if len(line) < self.column_width:
                    self.inline(line, end='\n')
                else:
                    line = f'{self.indentation()}std::optional<{field_type}>'
                    if len(line) < self.column_width:
                        self.inline(line, end='\n')
                        with self.indent():
                            self.write(f'{field_name};')
                    else:
                        self.write('std::optional<')
                        with self.indent():
                            self.write(field_type)
                        self.write(f'> {field_name};')
            else:
                line = f'{self.indentation()}{field_type} {field_name};'
                if len(line) < self.column_width:
                    self.inline(line, end='\n')
                else:
                    self.write(field_type)
                    with self.indent():
                        self.write(f'{field_name};')

    def generate_union_move_assign(self, union_name: str) -> None:
        self.gen_fn_decl(
            'operator=',
            f'{union_name} &',
            params=[
                f'{union_name} &&other'
            ],
        )

    def generate_union_copy_assign(self, union_name: str) -> None:
        self.gen_fn_decl(
            'operator=',
            f'{union_name} &',
            params=[
                f'const {union_name} &other'
            ],
        )

    def generate_union_field_copy_assign(
            self,
            union_name: str,
            field_name: str,
            field_spec: LspSpec
    ) -> None:
        field_type = self.get_type_declaration(field_spec)
        if self.is_union_pointer_type(field_spec):
            param = f'const {field_type} &{field_name}'
        else:
            param = f'{field_type} {field_name}'
        self.gen_fn_decl(
            'operator=',
            f'{union_name} &',
            params=[param],
        )

    def generate_union_field_move_assign(
            self,
            union_name: str,
            field_name: str,
            field_spec: LspSpec
    ) -> None:
        field_type = self.get_type_declaration(field_spec)
        self.gen_fn_decl(
            'operator=',
            f'{union_name} &',
            params=[
                f'{field_type} &&{field_name}'
            ],
        )

    def generate_union_field_getter(
            self,
            field_name: str,
            field_spec: LspSpec) -> None:
        field_type = self.get_type_declaration(field_spec)
        self.gen_fn_decl(field_name, f'const {field_type} &', specs='const')

    def generate_union_tag_getter(self, union_name: str) -> None:
        field_name = 'type'
        enum_name = f'{union_name}Type'
        self.gen_fn_decl(field_name, f'const {enum_name} &', specs='const')

    def generate_union_additional_pointer_setters(
            self,
            union_name: str,
            field_name: str,
            field_spec: LspSpec
    ) -> None:
        with self.type_declaration_options(
                wrap_ptr=True,
                wrap_nested_ptr=False,
                string_ptr=True,
                nested_string_ptr=False
        ):
            self.newline()
            self.generate_union_field_move_assign(union_name, field_name, field_spec)
        with self.type_declaration_options(
                wrap_ptr=False,
                wrap_nested_ptr=False,
                string_ptr=False,
                nested_string_ptr=False
        ):
            self.newline()
            self.generate_union_field_copy_assign(union_name, field_name, field_spec)
            self.newline()
            self.generate_union_field_move_assign(union_name, field_name, field_spec)

    def generate_union(self, union_symbol: LspSymbol) -> None:
        union_spec = union_symbol.spec
        union_name = union_symbol.name
        item_specs = union_spec["items"]
        has_pointer = any((self.is_union_pointer_type(item_spec)
                           for item_spec in item_specs))
        # Generate the field-type enum:
        # -----------------------------
        enum_name = f'{union_name}Type'
        self.gen_enum_with(enum_name, item_specs)
        self.newline()
        # Declare the mapping of field-type enums to their names:
        # -------------------------------------------------------
        names_by_enum = f'{enum_name}Names'
        self.gen_map_decl(names_by_enum, enum_name, 'std::string')
        self.newline()
        union_docs = union_spec.get("documentation", None)
        self.generate_docstring(union_docs)
        # Generate the struct:
        # --------------------
        with self.gen_class(union_name):
            with self.gen_public():
                self.write('// ======================================= //')
                self.write('// Constructor and Destructor Declarations //')
                self.write('// ======================================= //')
                self.newline()
                self.gen_constructor_decl(union_name)
                self.gen_move_constructor_decl(union_name)
                self.gen_copy_constructor_decl(union_name)
                self.gen_destructor_decl(union_name)
                self.newline()
                self.write('// =================== //')
                self.write('// Method Declarations //')
                self.write('// =================== //')
                self.newline()
                self.generate_union_move_assign(union_name)
                self.newline()
                self.generate_union_copy_assign(union_name)
                self.newline()
                self.generate_union_tag_getter(union_name)
                for item_index, item_spec in enumerate(item_specs):
                    with self.nest_name(str(item_index)):
                        field_name = self.name_field(item_spec)
                        with self.type_declaration_options(
                                wrap_ptr=False,
                                wrap_nested_ptr=False,
                                string_ptr=False,
                                nested_string_ptr=False
                        ):
                            self.newline()
                            self.generate_union_field_getter(field_name, item_spec)
                        with self.type_declaration_options(
                                wrap_ptr=True,
                                wrap_nested_ptr=False,
                                string_ptr=True,
                                nested_string_ptr=False
                        ):
                            self.newline()
                            self.generate_union_field_copy_assign(union_name, field_name, item_spec)
                        if self.is_union_pointer_type(item_spec):
                            self.generate_union_additional_pointer_setters(
                                union_name,
                                field_name,
                                item_spec
                            )
            with self.gen_private():
                self.write('// ================== //')
                self.write('// Field Declarations //')
                self.write('// ================== //')
                self.newline()
                self.gen_union_tag_field(enum_name)
                self.newline()
                with self.type_declaration_options(
                        wrap_ptr=True,
                        wrap_nested_ptr=False,
                        string_ptr=True,
                        nested_string_ptr=False,
                ):
                    self.gen_union_with(item_specs)
                if has_pointer:
                    self.newline()
                    self.gen_fn_decl('reset', 'void')
        self.newline()

    def gen_struct_move_assign_decl(self, struct_name: str) -> None:
        self.gen_fn_decl(
            'operator=',
            f'{struct_name} &',
            params=[
                f'{struct_name} &&other'
            ],
        )

    def gen_struct_copy_assign_decl(self, struct_name: str) -> None:
        self.gen_fn_decl(
            'operator=',
            f'{struct_name} &',
            params=[
                f'const {struct_name} &other'
            ],
        )

    def generate_structure(self, struct_symbol: LspSymbol) -> None:
        with self.type_declaration_options(
                wrap_ptr=False,
                wrap_nested_ptr=False,
                string_ptr=False,
                nested_string_ptr=False
        ):
            struct_spec = struct_symbol.spec
            struct_name = struct_symbol.name
            self.generate_docstring(struct_spec.get("documentation", None))
            extends_specs = struct_spec.get("extends", None)
            with self.gen_struct(struct_name, sups=extends_specs):
                self.write('// ======================================= //')
                self.write('// Constructor and Destructor Declarations //')
                self.write('// ======================================= //')
                self.newline()
                self.gen_constructor_decl(struct_name)
                self.gen_move_constructor_decl(struct_name)
                self.gen_copy_constructor_decl(struct_name)
                self.gen_destructor_decl(struct_name)
                self.newline()
                self.write('// =================== //')
                self.write('// Method Declarations //')
                self.write('// =================== //')
                self.newline()
                self.gen_struct_move_assign_decl(struct_name)
                self.newline()
                self.gen_struct_copy_assign_decl(struct_name)
                if struct_symbol.fields is not None and len(struct_symbol.fields) > 0:
                    self.newline()
                    self.write('// ================== //')
                    self.write('// Field Declarations //')
                    self.write('// ================== //')
                    self.newline()
                    for field_spec in struct_symbol.fields:
                        with self.nest_name(field_spec["name"]):
                            self.generate_docstring(
                                field_spec.get("documentation", None)
                            )
                            self.generate_struct_field(struct_symbol, field_spec)
            self.newline()

    def visit_alias_symbol(self, alias_symbol: LspSymbol) -> None:
        with self.nested_name_as(alias_symbol.name):
            with self.type_declaration_options(
                    wrap_ptr=False,
                    wrap_nested_ptr=False,
                    string_ptr=False,
                    nested_string_ptr=False,
            ):
                alias_spec = alias_symbol.spec
                alias_name = alias_spec["name"]
                type_spec = alias_spec["type"]
                type_name = type_spec["name"]
                type_symbol = self.resolve(type_name)
                self.write(f'typedef {type_symbol.resolved_name} {alias_name};')
                self.newline()

    def visit_base_symbol(self, base_symbol: LspSymbol) -> None:
        with self.nested_name_as(base_symbol.name):
            with self.type_declaration_options(
                    wrap_ptr=False,
                    wrap_nested_ptr=False,
                    string_ptr=False,
                    nested_string_ptr=False,
            ):
                base_name = base_symbol.name
                self.write('typedef {cxx_type} {lsp_type};'.format(
                    cxx_type=self.rename_base_type(base_name),
                    lsp_type=rename_type(base_name),
                ))
                self.newline()

    def visit_tuple_symbol(self, tuple_symbol: LspSymbol) -> None:
        with self.nested_name_as(tuple_symbol.name):
            with self.type_declaration_options(
                    wrap_ptr=False,
                    wrap_nested_ptr=False,
                    string_ptr=False,
                    nested_string_ptr=False,
            ):
                tuple_name = tuple_symbol.name
                tuple_spec = tuple_symbol.spec
                tuple_type = self.get_type_declaration(tuple_spec)
                self.write(f'typedef {tuple_type} {tuple_name};')
                self.newline()

    def visit_array_symbol(self, array_symbol: LspSymbol) -> None:
        with self.nested_name_as(array_symbol.name):
            with self.type_declaration_options(
                    wrap_ptr=False,
                    wrap_nested_ptr=False,
                    string_ptr=False,
                    nested_string_ptr=False,
            ):
                array_name = array_symbol.name
                array_spec = array_symbol.spec
                elem_spec = array_spec['element']
                with self.type_declaration_options(
                        wrap_nested_ptr=self.has_cycle(array_symbol, elem_spec)
                ):
                    array_type = self.get_type_declaration(array_spec)
                self.write(f'typedef {array_type} {array_name};')
                self.newline()

    def visit_map_symbol(self, map_symbol: LspSymbol) -> None:
        with self.nested_name_as(map_symbol.name):
            with self.type_declaration_options(
                    wrap_ptr=False,
                    wrap_nested_ptr=False,
                    string_ptr=False,
                    nested_string_ptr=False,
            ):
                map_name = map_symbol.name
                map_spec = map_symbol.spec
                value_spec = map_spec["value"]
                with self.type_declaration_options(
                        wrap_nested_ptr=self.has_cycle(map_symbol, value_spec)
                ):
                    map_type = self.get_type_declaration(map_spec)
                self.write(f'typedef {map_type} {map_name};')
                self.newline()

    def generate_enumeration(self, enum_spec: LspSpec) -> None:
        enum_name = enum_spec["name"]
        lower_enum = lower_first(enum_name)
        upper_enum = upper_first(enum_name)
        type_spec = enum_spec["type"]
        type_name = type_spec["name"]
        normalized_name = self.resolve(type_name).normalized_name
        is_string_type = (normalized_name == "string")
        value_type = self.rename_base_type(type_name)
        self.generate_docstring(enum_spec.get("documentation", None))
        with self.gen_enum(enum_name):
            if is_string_type:
                template = '{enum_name}'
            else:
                template = '{enum_name} = {enum_value}'
            for value in enum_spec["values"]:
                self.generate_docstring(value.get("documentation", None))
                expanded = template.format(
                    enum_name=rename_enum(value["name"]),
                    enum_value=value["value"]
                )
                self.write(f'{expanded},')
        self.newline()
        names_by_enum = f'{enum_name}Names'
        self.gen_map_decl(names_by_enum, enum_name, 'std::string')
        self.newline()
        if is_string_type:
            values_by_enum = f'{enum_name}Values'
            self.gen_map_decl(values_by_enum, enum_name, 'std::string')
            self.newline()
        enum_by_name = f'{lower_enum}ByName'
        self.gen_fn_decl(enum_by_name, enum_name, params=[
            'const std::string &name'
        ])
        self.newline()
        enum_by_value = f'{lower_enum}ByValue'
        if is_string_type:
            param = f'const {value_type} &value'
        else:
            param = f'{value_type} value'
        self.gen_fn_decl(enum_by_value, enum_name, params=[param])
        self.newline()
        if is_string_type:
            is_enum = f'is{upper_enum}'
            param = f'const {value_type} &value'
            self.gen_fn_decl(is_enum, 'bool', params=[param])
            self.newline()


    def recompute_dependency_dependencies(
            self,
            recomputed: Set[LspSymbol],
            symbol: LspSymbol
    ) -> None:
        if symbol.dependencies is not None:
            dependency_extractor = self.pipeline.dependency_extractor
            for dependency in symbol.dependencies:
                if dependency is not symbol and dependency not in recomputed:
                    dependency.dependencies = None
                    dependency_extractor.visit_symbol(dependency)
                    recomputed.add(dependency)
            symbol.dependencies = None
            dependency_extractor.visit_symbol(symbol)
            recomputed.add(symbol)
        else:
            raise ValueError(
                f'Cannot recompute dependency dependencies: symbol.dependencies is None'
            )

    def resolve_cycles(self) -> None:
        dependency_extractor = self.pipeline.dependency_extractor
        blacklist = dependency_extractor.blacklist
        cyclic_dependencies = dependency_extractor.cyclic_dependencies
        for symbol in cyclic_dependencies.keys():
            match symbol.kind:
                case LspSymbolKind.UNION:
                    self.write(f'class {symbol.name};  // Forward-declaration for cyclic dependencies')
                    self.newline()
                    blacklist.add(symbol)
                case LspSymbolKind.STRUCTURE | LspSymbolKind.INNER:
                    self.write(f'struct {symbol.name};  // Forward-declaration for cyclic dependencies')
                    self.newline()
                    blacklist.add(symbol)
        recomputed: Set[LspSymbol] = set()
        cyclic_dependencies.clear()
        for symbol in blacklist:
            self.recompute_dependency_dependencies(recomputed, symbol)
        num_cycles = len(cyclic_dependencies)
        if num_cycles > 0:
            raise RuntimeError(
                'Failed to resolve {length} cycles: [{names}]'.format(
                    length=num_cycles,
                    names=", ".join(map(lambda symbol: symbol.name, cyclic_dependencies))
                )
            )

    def generate_code(self) -> None:
        print(f'Generating: {self.file_path}')

        lsp_version: str = self.schema["metaData"]["version"]

        self.generate_disclaimer()
        self.pragma_once()
        self.newline()
        self.gen_include('cstddef')
        self.gen_include('map')
        self.gen_include('memory')
        self.gen_include('optional')
        self.gen_include('string')
        self.gen_include('tuple')
        self.gen_include('utility')
        self.gen_include('vector')
        self.newline()
        with self.gen_namespace(self.namespace):
            self.write('const std::string JSON_RPC_VERSION = "2.0";')
            self.write(f'const std::string LSP_VERSION = "{lsp_version}";')
            self.newline()
            self.resolve_cycles()
            super().generate_code()
            self.generate_request_enums()
            self.generate_notification_enums()
