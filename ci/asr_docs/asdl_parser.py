"""
ASDL Parser for ASR node definitions.

Parses ASR.asdl to extract node definitions, enums, and structs.
"""

from collections import defaultdict
import src.libasr.asdl as asdl


class ASDLParserVisitor(asdl.VisitorBase):
    """Visitor that extracts nodes, enums, and structs from ASDL."""

    def __init__(self):
        super().__init__()
        self.nodes = defaultdict(list)
        self.enums = defaultdict(list)
        self.structs = {}

    def visitModule(self, mod):
        for df in mod.dfns:
            self.visit(df)

    def visitType(self, tp):
        self.current_type = tp.name
        self.visit(tp.value)

    def visitSum(self, sum_):
        for cons in sum_.types:
            self.visit(cons)

    def visitConstructor(self, cons):
        if cons.fields:
            field_string = f"{cons.name}({', '.join(f'{f.type}' + ('*' if f.seq else '') + ('?' if f.opt else '') + ' ' + f.name for f in cons.fields)})"
            self.nodes[self.current_type].append((cons.name, field_string))
        else:
            self.enums[self.current_type].append(cons.name)

    def visitProduct(self, prod):
        field_string_list = []
        for f in prod.fields:
            type_str = f"{f.type}" + ('*' if f.seq else '') + ('?' if f.opt else '')
            field_string_list.append(type_str + " " + f.name)
        self.structs[self.current_type] = field_string_list


def parse_asdl(asdl_path):
    """
    Parse ASR.asdl and extract node definitions.

    Returns:
        tuple: (nodes, enums, structs) where:
            - nodes: dict mapping category -> list of (name, signature)
            - enums: dict mapping enum_type -> list of values
            - structs: dict mapping struct_name -> list of fields
    """
    mod = asdl.parse(asdl_path)
    visitor = ASDLParserVisitor()
    visitor.visit(mod)
    return visitor.nodes, visitor.enums, visitor.structs
