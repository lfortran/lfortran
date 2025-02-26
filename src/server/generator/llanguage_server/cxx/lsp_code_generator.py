import argparse
from typing import Any, Dict, Tuple

from llanguage_server.cxx.lsp_language_server_header_generator import \
    CPlusPlusLspLanguageServerHeaderGenerator
from llanguage_server.cxx.lsp_language_server_source_generator import \
    CPlusPlusLspLanguageServerSourceGenerator
from llanguage_server.cxx.lsp_specification_header_generator import \
    CPlusPlusSpecificationHeaderGenerator
from llanguage_server.cxx.lsp_specification_source_generator import \
    CPlusPlusSpecificationSourceGenerator
from llanguage_server.cxx.lsp_transformer_header_generator import \
    CPlusPlusLspTransformerHeaderGenerator
from llanguage_server.cxx.lsp_transformer_source_generator import \
    CPlusPlusLspTransformerSourceGenerator
from llanguage_server.lsp_code_generator import LspCodeGenerator


class CPlusPlusLspCodeGenerator(LspCodeGenerator):
    namespace: str = "LCompilers::LanguageServerProtocol"

    def __init__(self, args: argparse.Namespace) -> None:
        super().__init__(args)

    def generate_specification_header(self) -> None:
        code_generator = CPlusPlusSpecificationHeaderGenerator(
            self.args.output_dir,
            self.schema,
            self.pipeline,
            self.namespace
        )
        with code_generator.open():
            code_generator.generate_code()

    def generate_specification_source(self) -> None:
        code_generator = CPlusPlusSpecificationSourceGenerator(
            self.args.output_dir,
            self.schema,
            self.pipeline,
            self.namespace
        )
        with code_generator.open():
            code_generator.generate_code()

    def generate_specification(self) -> None:
        self.generate_specification_header()
        self.generate_specification_source()

    def generate_transformer_header(self) -> None:
        code_generator = CPlusPlusLspTransformerHeaderGenerator(
            self.args.output_dir,
            self.schema,
            self.pipeline,
            self.namespace
        )
        with code_generator.open():
            code_generator.generate_code()

    def generate_transformer_source(self) -> None:
        code_generator = CPlusPlusLspTransformerSourceGenerator(
            self.args.output_dir,
            self.schema,
            self.pipeline,
            self.namespace
        )
        with code_generator.open():
            code_generator.generate_code()

    def generate_server_header(self) -> None:
        code_generator = CPlusPlusLspLanguageServerHeaderGenerator(
            self.args.output_dir,
            self.schema,
            self.pipeline,
            self.namespace
        )
        with code_generator.open():
            code_generator.generate_code()

    def generate_server_source(self) -> None:
        code_generator = CPlusPlusLspLanguageServerSourceGenerator(
            self.args.output_dir,
            self.schema,
            self.pipeline,
            self.namespace
        )
        with code_generator.open():
            code_generator.generate_code()

    def generate_transformer(self) -> None:
        self.generate_transformer_header()
        self.generate_transformer_source()

    def generate_server(self) -> None:
        self.generate_server_header()
        self.generate_server_source()
