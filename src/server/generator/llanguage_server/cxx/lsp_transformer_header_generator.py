from collections import deque
from itertools import chain
from pathlib import Path
from typing import Any, Callable, Deque, Dict, Set, Tuple

from llanguage_server.auxiliary_schema import AUXILIARY_SCHEMA
from llanguage_server.cxx.file_generator import CPlusPlusLspFileGenerator
from llanguage_server.utils import (lower_first, method_to_camel_case,
                                    rename_type, upper_first)


class CPlusPlusLspTransformerHeaderGenerator(CPlusPlusLspFileGenerator):
    generated_to_any: Set[str]

    def __init__(
            self,
            output_dir: Path,
            schema: Dict[str, Any],
            namespace: str,
            symbols: Dict[str, Tuple[str, Dict[str, Any]]]
    ) -> None:
        specification_source = output_dir / "lsp_transformer.h"
        super().__init__(specification_source, schema, namespace, symbols)
        self.generated_to_any = set()

    def generate_as_message_params_type(self, type_spec: Dict[str, Any]) -> None:
        match type_spec["kind"]:
            case "reference":
                self.inline(f'std::unique_ptr<{type_spec["name"]}>')
            # case "array":
            #     self.inline('std::vector<')
            #     self.generate_request_params_type(spec["element"])
            #     self.inline('>')
            case _:
                raise ValueError(f'Unsupported request parameter type: {type_spec}')

    def generate_as_incoming_params(
            self,
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

    def generate_incoming_request_methods(self) -> None:
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
            self.write(f'auto asMessageParams(')
            with self.indent():
                self.write(f'const {params_spec["name"]} &{params_name}')
            self.write(') const -> MessageParams;')

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
                    symbol_name = result_name
                    symbol_spec = result_spec
                    symbol_kind = symbol_spec["kind"]
                    while (symbol_kind == "reference") and (symbol_name in self.symbols):
                        symbol_kind, symbol_spec = self.symbols[symbol_name]
                        symbol_name = symbol_spec["name"]
                    self.write(f'auto anyTo{upper_first(result_name)}(')
                    with self.indent():
                        self.write(f'const LSPAny &any')
                    if symbol_kind == "structure":
                        self.write(f') const -> std::unique_ptr<{result_name}>;')
                    else:
                        self.write(f') const -> {result_name};')

    def generate_outgoing_notification(self) -> None:
        self.write('// ====================== //')
        self.write('// Outgoing Notifications //')
        self.write('// ====================== //')
        self.newline()
        for notification_spec in self.schema["notifications"]:
            if notification_spec["messageDirection"] == "serverToClient":
                self.generate_as_outgoing_params("notificationParams", notification_spec)

    def generate_enumeration_transforms(self) -> None:
        self.write('// ===================================== //')
        self.write('// LSPAny <-> LSP Enumeration Transforms //')
        self.write('// ===================================== //')
        self.newline()
        enum_specs = chain(
            AUXILIARY_SCHEMA["enumerations"],
            self.schema["enumerations"],
        )
        for enum_spec in enum_specs:
            enum_name = enum_spec["name"]
            self.write(f'auto anyTo{upper_first(enum_name)}(')
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

    def generate_nested_variant(
        self,
        nested_names: Deque[str],
        spec: Dict[str, Any]
    ) -> None:
        with self.nested_names_as(nested_names) as nested_name:
            self.write(f'auto anyTo{upper_first(nested_name)}(')
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
        self,
        nested_names: Deque[str],
        spec: Dict[str, Any]
    ) -> None:
        with self.nested_names_as(nested_names) as nested_name:
            self.write(f'auto anyTo{upper_first(nested_name)}(')
            with self.indent():
                self.write('const LSPAny &any')
            self.write(f') const -> std::unique_ptr<{nested_name}>;')
            fn_nym = f'{lower_first(nested_name)}ToAny'
            self.write(f'auto {fn_nym}(')
            with self.indent():
                self.write(f'const {nested_name} &structure')
            self.write(f') const -> std::unique_ptr<LSPAny>;')
            self.generated_to_any.add(fn_nym)

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
                            self.write(f'auto anyTo{upper_first(alias_name)}(')
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

    def generate_copy_methods(self) -> None:
        self.write('// ============ //')
        self.write('// Copy Methods //')
        self.write('// ============ //')
        self.newline()
        self.write('auto copy(const std::unique_ptr<LSPAny> &any) const -> std::unique_ptr<LSPAny>;')
        self.write('auto copy(const LSPObject &object) const -> LSPObject;')
        self.write('auto copy(const LSPArray &array) const -> LSPArray;')

    def generate_code(self) -> None:
        print(f'Generating: {self.file_path}')
        self.generate_disclaimer()
        self.newline()
        self.write('#pragma once')
        self.newline()
        self.write('#include <cstddef>')
        self.write('#include <memory>')
        self.newline()
        self.write('#include <server/specification.h>')
        self.newline()
        self.write(f'namespace {self.namespace} {{')
        self.newline()
        with self.indent():
            self.write('class LspTransformer {')
            self.write('public:')
            with self.indent():
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
            self.write('}; // class LspTransformer')
            self.newline()
        self.write(f'}} // namespace {self.namespace}')
