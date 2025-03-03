from pathlib import Path
from typing import Optional

from llanguage_server.cxx.visitors import BaseCPlusPlusLspVisitor
from llanguage_server.lsp.datatypes import LspSpec, LspSymbol
from llanguage_server.lsp.utils import (lower_first, method_to_camel_case,
                                        rename_type, upper_first)
from llanguage_server.lsp.visitors import LspAnalysisPipeline


class CPlusPlusLspTransformerHeaderGenerator(BaseCPlusPlusLspVisitor):

    def __init__(
            self,
            output_dir: Path,
            schema: LspSpec,
            pipeline: LspAnalysisPipeline,
            namespace: str
    ) -> None:
        specification_source = output_dir / "lsp_transformer.h"
        super().__init__(specification_source, schema, pipeline, namespace)

    def get_as_message_params_type(self, type_spec: LspSpec) -> str:
        match type_spec["kind"]:
            case "reference":
                return type_spec["name"]
            case _:
                raise ValueError(f'Unsupported request parameter type: {type_spec}')

    def generate_as_incoming_params(
            self,
            params_name: str,
            message_spec: LspSpec
    ) -> None:
        message_name = method_to_camel_case(message_spec["method"])
        params_spec = message_spec.get("params", None)
        if params_spec is not None:
            as_message_params = f'as{message_name}Params'
            return_type = self.get_as_message_params_type(params_spec)
            self.gen_fn_decl(
                as_message_params,
                return_type,
                params=[
                    f'const MessageParams &{params_name}',
                ],
                specs='const'
            )

    def generate_as_outgoing_params(
        self,
        params_name: str,
        message_spec: LspSpec
    ) -> None:
        params_spec = message_spec.get("params", None)
        if params_spec is not None:
            self.write(f'auto asMessageParams(')
            with self.indent():
                self.write(f'const {params_spec["name"]} &{params_name}')
            self.write(') const -> MessageParams;')

    def visit_request_symbol(self, request_symbol: LspSymbol) -> None:
        request_spec = request_symbol.spec
        match request_spec["messageDirection"]:
            case "clientToServer":
                self.generate_as_incoming_params("requestParams", request_spec)
            case "serverToClient":
                self.generate_as_outgoing_params("requestParams", request_spec)
            case "both":
                self.generate_as_incoming_params("requestParams", request_spec)
                self.generate_as_outgoing_params("requestParams", request_spec)
            case _:
                raise ValueError(
                    f'Unsupported messageDirection: {request_spec["messageDirection"]}'
                )

    def visit_notification_symbol(self, notification_symbol: LspSymbol) -> None:
        notification_spec = notification_symbol.spec
        match notification_spec["messageDirection"]:
            case "clientToServer":
                self.generate_as_incoming_params("notificationParams", notification_spec)
            case "serverToClient":
                self.generate_as_outgoing_params("notificationParams", notification_spec)
            case "both":
                self.generate_as_incoming_params("notificationParams", notification_spec)
                self.generate_as_outgoing_params("notificationParams", notification_spec)
            case _:
                raise ValueError(
                    f'Unsupported messageDirection: {notification_spec["messageDirection"]}'
                )

    def gen_type_to_and_from_any(
            self,
            type_name: str,
            type_param: str,
            return_type: Optional[str] = None
    ) -> None:
        if return_type is None:
            return_type = type_name
        upper_type = upper_first(type_name)
        lower_type = lower_first(type_name)
        any_to_type = f'anyTo{upper_type}'
        type_to_any = f'{lower_type}ToAny'
        self.gen_fn_decl(
            any_to_type,
            return_type,
            params=[
                'const LSPAny &any',
            ],
            specs='const'
        )
        self.gen_fn_decl(
            type_to_any,
            'LSPAny',
            params=[
                type_param,
            ],
            specs='const'
        )

    def generate_enumeration(self, enum_spec: LspSpec) -> None:
        enum_name = enum_spec["name"]
        enum_param = f'{enum_name} param'
        self.gen_type_to_and_from_any(enum_name, enum_param)

    def generate_union(self, union_symbol: LspSymbol) -> None:
        union_name = union_symbol.name
        union_param = f'const {union_name} &param'
        self.gen_type_to_and_from_any(union_name, union_param)

    def generate_structure(self, struct_symbol: LspSymbol) -> None:
        struct_name = struct_symbol.name
        struct_param = f'const {struct_name} &param'
        self.gen_type_to_and_from_any(struct_name, struct_param)

    def visit_alias_symbol(self, alias_symbol: LspSymbol) -> None:
        alias_name = alias_symbol.name
        alias_type = rename_type(alias_name)
        alias_param = f'const {alias_type} &param'
        self.gen_type_to_and_from_any(alias_name, alias_param, alias_type)

    def visit_base_symbol(self, base_symbol: LspSymbol) -> None:
        base_name = base_symbol.name
        base_type = rename_type(base_name)
        if base_symbol.normalized_name == "string":
            base_param = f'const {base_type} &param'
        else:
            base_param = f'{base_type} param'
        self.gen_type_to_and_from_any(base_name, base_param, base_type)

    def visit_array_symbol(self, array_symbol: LspSymbol) -> None:
        array_name = array_symbol.name
        array_type = rename_type(array_name)
        array_param = f'const {array_symbol.name} &array'
        self.gen_type_to_and_from_any(array_name, array_param, array_type)

    def visit_map_symbol(self, map_symbol: LspSymbol) -> None:
        map_name = map_symbol.name
        map_type = rename_type(map_name)
        map_param = f'const {map_symbol.name} &map'
        self.gen_type_to_and_from_any(map_name, map_param, map_type)

    def generate_copy_methods(self) -> None:
        self.gen_fn_decl(
            'copy',
            'LSPAny',
            params=[
                'const LSPAny &any',
            ],
            specs='const'
        )
        self.gen_fn_decl(
            'copy',
            'std::unique_ptr<LSPAny>',
            params=[
                'const std::unique_ptr<LSPAny> &any',
            ],
            specs='const'
        )
        self.gen_fn_decl(
            'copy',
            'LSPObject',
            params=[
                'const LSPObject &object',
            ],
            specs='const'
        )
        self.gen_fn_decl(
            'copy',
            'LSPArray',
            params=[
                'const LSPArray &array',
            ],
            specs='const'
        )

    def generate_code(self) -> None:
        print(f'Generating: {self.file_path}')
        self.generate_disclaimer()
        self.pragma_once()
        self.newline()
        self.gen_include('memory')
        self.newline()
        self.gen_include('server/lsp_specification.h')
        self.newline()
        with self.gen_namespace(self.namespace):
            self.newline()
            with self.gen_class('LspTransformer'):
                with self.gen_public():
                    super().generate_code()
                    self.generate_copy_methods()
            self.newline()
