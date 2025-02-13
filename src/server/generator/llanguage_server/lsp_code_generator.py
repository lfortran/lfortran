import argparse
import json
from abc import ABC, abstractmethod
from typing import (
    Any,
    Dict,
)

class LspCodeGenerator(ABC):
    args: argparse.Namespace
    schema: Dict[str, Any]

    def __init__(self, args: argparse.Namespace) -> None:
        self.args = args

    def generate_code(self) -> None:
        print('Creating parent directories')
        self.args.output_dir.mkdir(parents=True, exist_ok=True)
        print('Loading schema')
        self.schema = json.loads(self.args.schema.read())
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
