import argparse
import json
from abc import ABC, abstractmethod

from llanguage_server.lsp.auxiliary_schema import AUXILIARY_SCHEMA
from llanguage_server.lsp.visitors import LspAnalysisPipeline
from llanguage_server.lsp.datatypes import LspSpec


class LspCodeGenerator(ABC):
    args: argparse.Namespace
    schema: LspSpec
    pipeline: LspAnalysisPipeline

    def __init__(self, args: argparse.Namespace) -> None:
        self.args = args

    def generate_code(self) -> None:
        print('Creating parent directories')
        self.args.output_dir.mkdir(parents=True, exist_ok=True)

        print('Loading schema')
        self.schema = json.loads(self.args.schema.read())
        for key, values in AUXILIARY_SCHEMA.items():
            self.schema[key].extend(values)

        print('Preprocessing data types')
        self.pipeline = LspAnalysisPipeline(self.schema)
        self.pipeline.run()

        print('Generating files')
        self.generate_specification()
        self.generate_transformer()
        self.generate_server()

        print('Done.')

    @abstractmethod
    def generate_specification(self) -> None:
        raise NotImplementedError()

    @abstractmethod
    def generate_transformer(self) -> None:
        raise NotImplementedError()

    @abstractmethod
    def generate_server(self) -> None:
        raise NotImplementedError()
