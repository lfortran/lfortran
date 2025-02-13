import argparse
from typing import (
    Any,
    Dict,
    Tuple,
)

from llanguage_server.lsp_code_generator import LspCodeGenerator
from llanguage_server.cxx.lsp_language_server_header_generator import CPlusPlusLspLanguageServerHeaderGenerator
from llanguage_server.cxx.lsp_language_server_source_generator import CPlusPlusLspLanguageServerSourceGenerator
from llanguage_server.cxx.lsp_transformer_header_generator import CPlusPlusLspTransformerHeaderGenerator
from llanguage_server.cxx.lsp_transformer_source_generator import CPlusPlusLspTransformerSourceGenerator
from llanguage_server.cxx.specification_header_generator import CPlusPlusSpecificationHeaderGenerator
from llanguage_server.cxx.specification_source_generator import CPlusPlusSpecificationSourceGenerator

class CPlusPlusLspCodeGenerator(LspCodeGenerator):
    namespace: str = "LCompilers::LanguageServerProtocol"
    symbols: Dict[str, Tuple[str, Dict[str, Any]]]

    def __init__(self, args: argparse.Namespace) -> None:
        super().__init__(args)
        self.symbols = {}

    def generate_specification_header(self) -> None:
        code_generator = CPlusPlusSpecificationHeaderGenerator(
            self.args.output_dir,
            self.schema,
            self.namespace,
            self.symbols
        )
        with code_generator.open():
            code_generator.generate_code()

    def generate_specification_source(self) -> None:
        code_generator = CPlusPlusSpecificationSourceGenerator(
            self.args.output_dir,
            self.schema,
            self.namespace,
            self.symbols
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
            self.namespace,
            self.symbols
        )
        with code_generator.open():
            code_generator.generate_code()

    def generate_transformer_source(self) -> None:
        code_generator = CPlusPlusLspTransformerSourceGenerator(
            self.args.output_dir,
            self.schema,
            self.namespace,
            self.symbols
        )
        with code_generator.open():
            code_generator.generate_code()

    def generate_server_header(self) -> None:
        code_generator = CPlusPlusLspLanguageServerHeaderGenerator(
            self.args.output_dir,
            self.schema,
            self.namespace,
            self.symbols
        )
        with code_generator.open():
            code_generator.generate_code()

    def generate_server_source(self) -> None:
        code_generator = CPlusPlusLspLanguageServerSourceGenerator(
            self.args.output_dir,
            self.schema,
            self.namespace,
            self.symbols
        )
        with code_generator.open():
            code_generator.generate_code()

    def generate_transformer(self) -> None:
        self.generate_transformer_header()
        self.generate_transformer_source()

    def generate_server(self) -> None:
        self.generate_server_header()
        self.generate_server_source()
