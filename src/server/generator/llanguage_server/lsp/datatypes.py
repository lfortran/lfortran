from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Dict, List, Optional, Set

from llanguage_server.lsp.utils import normalize_name, rename_type

LspSpec = Dict[str, Any]


class LspSymbolKind(str, Enum):
    ALIAS = 'alias'
    ARRAY = 'array'
    BASE = 'base'
    ENUMERATION = 'enumeration'
    INNER = 'inner'
    MAP = 'map'
    NOTIFICATION = 'notification'
    REFERENCE = 'reference'
    REQUEST = 'request'
    STRUCTURE = 'structure'
    TUPLE = 'tuple'
    UNION = 'union'


@dataclass
class LspSymbol:
    name: str
    kind: LspSymbolKind
    spec: LspSpec
    dependencies: Optional[List["LspSymbol"]] = None
    fields: Optional[List[LspSpec]] = None
    resolution: Optional["LspSymbol"] = None
    normalized_name: str = field(init=False)
    resolved_name: str = field(init=False)
    cycles: Optional[Set["LspSymbol"]] = None

    def __post_init__(self) -> None:
        self.normalized_name = normalize_name(self.name)
        self.resolved_name = rename_type(self.name)

    def __hash__(self) -> int:
        return hash(self.name)

    def __eq__(self, other: Any) -> bool:
        if isinstance(other, LspSymbol):
            return self.name == other.name
        return False
