from abc import abstractmethod
from pathlib import Path
from typing import Dict, Tuple

from llanguage_server.cxx.lsp_file_generator import CPlusPlusLspFileGenerator
from llanguage_server.lsp.datatypes import LspSpec, LspSymbol
from llanguage_server.lsp.visitors import (BaseLspSymbolVisitor,
                                           LspAnalysisPipeline)


class BaseCPlusPlusLspVisitor(CPlusPlusLspFileGenerator, BaseLspSymbolVisitor):

    def __init__(
            self,
            file_path: Path,
            schema: LspSpec,
            pipeline: LspAnalysisPipeline,
            namespace: str
    ) -> None:
        CPlusPlusLspFileGenerator.__init__(self, file_path, schema, pipeline, namespace)
        BaseLspSymbolVisitor.__init__(self, pipeline.indexer)

    def generate_structure(self, struct_symbol: LspSymbol) -> None:
        pass

    def generate_union(self, union_symbol: LspSymbol) -> None:
        pass

    def visit_alias_symbol(self, alias_symbol: LspSymbol) -> None:
        pass

    def visit_array_symbol(self, array_symbol: LspSymbol) -> None:
        pass

    def visit_base_symbol(self, base_symbol: LspSymbol) -> None:
        pass

    def visit_enumeration_symbol(self, enum_symbol: LspSymbol) -> None:
        with self.nested_name_as(enum_symbol.name):
            enum_spec = enum_symbol.spec
            self.generate_enumeration(enum_spec)

    def visit_inner_symbol(self, inner_symbol: LspSymbol) -> None:
        with self.nested_name_as(inner_symbol.name):
            self.generate_structure(inner_symbol)

    def visit_map_symbol(self, map_symbol: LspSymbol) -> None:
        pass

    def visit_notification_symbol(self, notification_symbol: LspSymbol) -> None:
        pass

    def visit_reference_symbol(self, reference_symbol: LspSymbol) -> None:
        pass

    def visit_request_symbol(self, request_symbol: LspSymbol) -> None:
        pass

    def visit_structure_symbol(self, struct_symbol: LspSymbol) -> None:
        with self.nested_name_as(struct_symbol.name):
            self.generate_structure(struct_symbol)

    def visit_tuple_symbol(self, tuple_symbol: LspSymbol) -> None:
        pass

    def visit_union_symbol(self, union_symbol: LspSymbol) -> None:
        with self.nested_name_as(union_symbol.name):
            self.generate_union(union_symbol)

    def generate_code(self) -> None:
        for symbol in self.pipeline.indexer.topological_sort():
            self.visit_symbol(symbol)
