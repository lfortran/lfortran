from pathlib import Path
from typing import (
    Any,
    Dict,
    Optional,
    Tuple,
)

from llanguage_server.file_generator import FileGenerator
from llanguage_server.utils import rename_enum, rename_type

class CPlusPlusLspFileGenerator(FileGenerator):
    namespace: str
    symbols: Dict[str, Tuple[str, Dict[str, Any]]]

    def __init__(
        self,
        output_path: Path,
        schema: Dict[str, Any],
        namespace: str,
        symbols: Dict[str, Tuple[str, Dict[str, Any]]]
    ) -> None:
        super().__init__(output_path, schema)
        self.namespace = namespace
        self.symbols = symbols

    def generate_disclaimer(self) -> None:
        self.write('// ------------------------------------------------------------------------------')
        self.write('// NOTE: This file was generated from Microsoft\'s Language Server Protocol (LSP)')
        self.write('// specification. Please do not edit it by hand.')
        self.write('// ------------------------------------------------------------------------------')
        self.newline()

    def generate_docstring(self, docs: Optional[str]) -> None:
        if docs is not None:
            docs = docs.replace(r'/*', '/​*')
            self.write( '/**')
            for line in docs.split('\n'):
                self.inline(' *', indent=True)
                if len(line) > 0:
                    self.inline(f' {line}')
                self.newline()
            self.write( ' */')

    def generate_variant_enumeration(
        self,
        spec: Dict[str, Any]
    ) -> None:
        match spec["kind"]:
            case "base" | "reference":
                self.inline(rename_enum(spec["name"]))
            case "stringLiteral":
                self.inline(rename_enum("string"))
            case "integerLiteral":
                self.inline(rename_enum("integer"))
            case "booleanLiteral":
                self.inline(rename_enum("boolean"))
            case "array":
                self.generate_variant_enumeration(spec["element"])
                self.inline("_ARRAY");
            case "map":
                self.generate_variant_enumeration(spec["key"])
                self.generate_variant_enumeration(spec["value"])
            case "and":
                raise ValueError(
                    f'AND types are not supported for type declarations: {spec}'
                )
            case "or":
                for index, item_spec in enumerate(spec["items"]):
                    with self.nest_name(str(index)):
                        if index > 0:
                            self.inline('_OR_')
                        self.generate_variant_enumeration(item_spec)
            case "tuple":
                item_specs = spec["items"]
                if len(item_specs) == 2:
                    self.inline('PAIR_OF_')
                else:
                    self.inline('PAIR_OF_')
                for index, item_spec in enumerate(item_specs):
                    if index > 0:
                        self.inline('_AND_')
                    self.generate_variant_enumeration(item_spec)
            case "literal":
                self.inline(rename_enum(self.nested_name()))
            case _:
                raise ValueError(f'Unsupported variant enumeration kind ({spec["kind"]}): {spec}')

    def generate_type_declaration(self, type_spec: Dict[str, Any]) -> None:
        match type_spec["kind"]:
            case "base":
                type_name = type_spec["name"]
                self.inline(rename_type(type_name))
            case "reference":
                type_name = type_spec["name"]
                match type_name:
                    case "LSPObject" | "LSPArray":
                        self.inline(type_name)
                    case "LSPAny":
                        self.inline(f'std::unique_ptr<{type_name}>')
                    case _:
                        symbol_kind, symbol_spec = self.symbols[type_name]
                        if symbol_kind == "alias":
                            symbol_type = symbol_spec["type"]
                            match symbol_type["kind"]:
                                case "base" | "array":
                                    self.inline(type_name)
                                case "reference":
                                    self.inline(f'std::unique_ptr<{type_name}>')
                                case _:
                                    raise ValueError(f'Unsupported alias type ({symbol_type["kind"]}): {symbol_spec}')
                        elif symbol_kind == "structure":
                            self.inline(f'std::unique_ptr<{type_name}>')
                        else:
                            self.inline(type_name)
            case "array":
                self.inline('std::vector<')
                self.generate_type_declaration(type_spec["element"])
                self.inline('>')
            case "map":
                self.inline('std::map<')
                self.generate_type_declaration(type_spec["key"])
                self.inline(', ')
                self.generate_type_declaration(type_spec["value"])
                self.inline('>')
            case "and":
                raise ValueError(f'AND types are not supported for type declarations: {type_spec}')
            case "or":
                self.inline(self.nested_name())
            case "tuple":
                item_specs = type_spec["items"]
                if len(item_specs) == 2:
                    self.inline('std::pair<')
                else:
                    self.inline('std::tuple<')
                k = len(item_specs)
                if k > 0:
                    self.generate_type_declaration(item_specs[0])
                    if k > 1:
                        for i in range(1, k):
                            self.inline(', ')
                            self.generate_type_declaration(item_specs[i])
                self.inline('>')
            case "literal":
                self.inline(f'std::unique_ptr<{self.nested_name()}>')
            case "stringLiteral":
                self.inline(rename_type('string'))
            case "integerLiteral":
                self.inline(rename_type('integer'))
            case "booleanLiteral":
                self.inline(rename_type('boolean'))
            case _:
                raise ValueError(f'Unsupported Type kind: {type_spec}')
