from abc import ABC, abstractmethod
from collections import OrderedDict, deque
from contextlib import contextmanager
from functools import wraps
from itertools import chain
from typing import (Callable, Collection, Deque, Dict, Generic, Iterable,
                    Iterator, List, Optional, Set, TypeVar, Union)

from llanguage_server.lsp.datatypes import LspSpec, LspSymbol, LspSymbolKind
from llanguage_server.lsp.utils import method_to_camel_case


def memoize_instance_unary(fn: Callable) -> Callable:
    memo = {}
    @wraps(fn)
    def wrapper(self, arg):
        if arg in memo:
            return memo[arg]
        val = fn(self, arg)
        memo[arg] = val
        return val
    return wrapper


T = TypeVar('T')


class LspSpecVisitor(ABC, Generic[T]):

    def visit_type(self, type_spec: LspSpec) -> T:
        match type_spec["kind"]:
            case "base":
                return self.visit_base(type_spec)
            case "reference":
                return self.visit_reference(type_spec)
            case "enumeration":
                return self.visit_enumeration(type_spec)
            case "array":
                return self.visit_array(type_spec)
            case "map":
                return self.visit_map(type_spec)
            case "and":
                return self.visit_and(type_spec)
            case "or":
                return self.visit_union(type_spec)
            case "tuple":
                return self.visit_tuple(type_spec)
            case "literal":
                return self.visit_inner(type_spec)
            case "stringLiteral":
                return self.visit_string_literal(type_spec)
            case "integerLiteral":
                return self.visit_integer_literal(type_spec)
            case "booleanLiteral":
                return self.visit_boolean_literal(type_spec)
            case _:
                raise ValueError(f'Unsupported Type kind: {type_spec}')

    @abstractmethod
    def visit_base(self, base_spec: LspSpec) -> T:
        raise NotImplementedError

    @abstractmethod
    def visit_reference(self, reference_spec: LspSpec) -> T:
        raise NotImplementedError

    @abstractmethod
    def visit_enumeration(self, enum_spec: LspSpec) -> T:
        raise NotImplementedError

    @abstractmethod
    def visit_array(self, array_spec: LspSpec) -> T:
        raise NotImplementedError

    @abstractmethod
    def visit_map(self, map_spec: LspSpec) -> T:
        raise NotImplementedError

    @abstractmethod
    def visit_and(self, and_spec: LspSpec) -> T:
        raise NotImplementedError

    @abstractmethod
    def visit_union(self, union_spec: LspSpec) -> T:
        raise NotImplementedError

    @abstractmethod
    def visit_tuple(self, tuple_spec: LspSpec) -> T:
        raise NotImplementedError

    @abstractmethod
    def visit_inner(self, inner_spec: LspSpec) -> T:
        raise NotImplementedError

    @abstractmethod
    def visit_string_literal(self, string_spec: LspSpec) -> T:
        raise NotImplementedError

    @abstractmethod
    def visit_integer_literal(self, integer_spec: LspSpec) -> T:
        raise NotImplementedError

    @abstractmethod
    def visit_boolean_literal(self, boolean_spec: LspSpec) -> T:
        raise NotImplementedError


class NestedContext:
    nested_names: Deque[str]

    def __init__(self) -> None:
        self.nested_names = deque()

    @contextmanager
    def nested_names_as(self, nested_names: Collection[str]) -> Iterator[str]:
        if not isinstance(nested_names, deque):
            nested_names = deque(nested_names)
        prev_nested_names = self.nested_names
        self.nested_names = nested_names
        yield self.nested_name()
        self.nested_names = prev_nested_names

    @contextmanager
    def nested_name_as(self, next_nested_name: str) -> Iterator[str]:
        with self.nested_names_as(deque([next_nested_name])) as nested_name:
            yield nested_name

    @contextmanager
    def nest_name(self, name: str) -> Iterator[str]:
        self.nested_names.append(name)
        yield self.nested_name()
        self.nested_names.pop()

    def nested_name(self, nested_names: Optional[Collection[str]] = None) -> str:
        if nested_names is None:
            nested_names = self.nested_names
        return "_".join(nested_names)


class BaseLspSpecVisitor(LspSpecVisitor[None], NestedContext):

    def __init__(self) -> None:
        LspSpecVisitor.__init__(self)
        NestedContext.__init__(self)

    def visit_base(self, base_spec: LspSpec) -> None:
        pass

    def visit_reference(self, reference_spec: LspSpec) -> None:
        pass

    def visit_enumeration(self, enum_spec: LspSpec) -> None:
        pass

    def visit_array(self, array_spec: LspSpec) -> None:
        with self.nest_name('elem'):
            elem_spec: LspSpec = array_spec["element"]
            self.visit_type(elem_spec)

    def visit_map(self, map_spec: LspSpec) -> None:
        key_spec: LspSpec = map_spec["key"]
        value_spec: LspSpec = map_spec["value"]
        with self.nest_name('key') as key_name:
            self.visit_type(key_spec)
        with self.nest_name('value') as value_name:
            self.visit_type(value_spec)

    def visit_and(self, and_spec: LspSpec) -> None:
        item_specs: List[LspSpec] = and_spec["items"]
        for item_spec in item_specs:
            self.visit_type(item_spec)

    def visit_union(self, union_spec: LspSpec) -> None:
        item_specs: List[LspSpec] = union_spec["items"]
        for union_index, item_spec in enumerate(item_specs):
            with self.nest_name(str(union_index)) as nested_name:
                self.visit_type(item_spec)

    def visit_tuple(self, tuple_spec: LspSpec) -> None:
        tuple_items: List[LspSpec] = tuple_spec["items"]
        for tuple_index, tuple_item in enumerate(tuple_items):
            with self.nest_name(str(tuple_index)) as nested_name:
                self.visit_type(tuple_item)

    def visit_inner(self, inner_spec: LspSpec) -> None:
        value_spec: LspSpec = inner_spec["value"]
        field_specs: List[LspSpec] = value_spec["properties"]
        for field_spec in field_specs:
            self.visit_field(field_spec)

    def visit_field(self, field_spec: LspSpec) -> None:
        field_name = field_spec["name"]
        with self.nest_name(field_name) as nested_name:
            type_spec = field_spec["type"]
            self.visit_type(type_spec)

    def visit_string_literal(self, string_spec: LspSpec) -> None:
        pass

    def visit_integer_literal(self, integer_spec: LspSpec) -> None:
        pass

    def visit_boolean_literal(self, boolean_spec: LspSpec) -> None:
        pass

    def visit_structure(self, struct_spec: LspSpec) -> None:
        struct_name: str = struct_spec["name"]
        with self.nested_name_as(struct_name):
            super_specs: Optional[List[LspSpec]] = struct_spec.get("extends", None)
            if super_specs is not None:
                for super_spec in super_specs:
                    self.visit_type(super_spec)
            mixin_specs: Optional[List[LspSpec]] = struct_spec.get("mixins", None)
            if mixin_specs is not None:
                for mixin_spec in mixin_specs:
                    self.visit_type(mixin_spec)
            field_specs: List[LspSpec] = struct_spec["properties"]
            for field_spec in field_specs:
                self.visit_field(field_spec)

    def visit_type_alias(self, alias_spec: LspSpec) -> None:
        alias_name = alias_spec["name"]
        with self.nested_name_as(alias_name):
            type_spec: LspSpec = alias_spec["type"]
            self.visit_type(type_spec)

    def visit_request(self, request_spec: LspSpec) -> None:
        request_method = request_spec["method"]
        request_params: Optional[LspSpec] = request_spec.get("params", None)
        if request_params is not None:
            self.visit_type(request_params)
        request_name = method_to_camel_case(request_method)
        result_name = f'{request_name}Result'
        with self.nested_name_as(result_name):
            result_spec = request_spec["result"]
            # self.visit_type(result_spec)
            partial_result_spec = request_spec.get("partialResult", None)
            if partial_result_spec is not None:
                # self.visit_type(partial_result_spec)
                if result_spec["kind"] == partial_result_spec["kind"] == "or":
                    result_types = list(result_spec["items"])
                    for item_spec in partial_result_spec["items"]:
                        if item_spec not in result_types:
                            result_types.append(item_spec)
                elif result_spec["kind"] == "or":
                    result_types = list(result_spec["items"])
                    if partial_result_spec not in result_types:
                        result_types.append(partial_result_spec)
                elif partial_result_spec["kind"] == "or":
                    result_types = list(partial_result_spec["items"])
                    if result_spec not in result_types:
                        result_types.append(result_spec)
                else:
                    result_types = [result_spec]
                    if partial_result_spec not in result_types:
                        result_types.append(partial_result_spec)
            elif result_spec["kind"] == "or":
                result_types = result_spec["items"]
            else:
                result_types = [result_spec]
            if len(result_types) > 1:
                    union_type = {
                        "kind": "or",
                        "items": result_types
                    }
                    self.visit_union(union_type)
            else:
                alias_spec: LspSpec = {
                    "name": result_name,
                    "type": result_types[0]
                }
                self.visit_type_alias(alias_spec)

    def visit_notification(self, notification_spec: LspSpec) -> None:
        notification_params: Optional[LspSpec] = notification_spec.get("params", None)
        if notification_params is not None:
            self.visit_type(notification_params)


LspNameIndex = Dict[str, LspSymbol]
LspKindIndex = Dict[LspSymbolKind, List[LspSymbol]]
LspMessageIndex = Dict[str, List[LspSymbol]]

class LspIndexer(BaseLspSpecVisitor):
    schema: LspSpec

    name_index: LspNameIndex
    kind_index: LspKindIndex
    request_index: LspMessageIndex
    notification_index: LspMessageIndex

    def __init__(self, schema: LspSpec) -> None:
        BaseLspSpecVisitor.__init__(self)
        self.schema = schema
        self.name_index = OrderedDict()
        self.kind_index = OrderedDict()
        self.request_index = OrderedDict()
        self.notification_index = OrderedDict()

    def index_by_name(self, name: str, symbol: LspSymbol) -> None:
        self.name_index[name] = symbol

    def index_by_kind(self, kind: LspSymbolKind, symbol: LspSymbol) -> None:
        if kind in self.kind_index:
            symbols = self.kind_index[kind]
        else:
            symbols = []
            self.kind_index[kind] = symbols
        symbols.append(symbol)

    def index_request_by_direction(self, direction: str, symbol: LspSymbol) -> None:
        if direction in self.request_index:
            requests = self.request_index[direction]
        else:
            requests = []
            self.request_index[direction] = requests
        requests.append(symbol)

    def index_notification_by_direction(self, direction: str, symbol: LspSymbol) -> None:
        if direction in self.notification_index:
            notifications = self.notification_index[direction]
        else:
            notifications = []
            self.notification_index[direction] = notifications
        notifications.append(symbol)

    def topological_sort(self) -> Iterable[LspSymbol]:
        return LspTopologicalIterator(self)

    def index(self) -> None:
        self.visit_enumerations()
        self.visit_structures()
        self.visit_type_aliases()
        self.visit_requests()
        self.visit_notifications()

    def visit_enumerations(self) -> None:
        for enum_spec in self.schema["enumerations"]:
            self.visit_enumeration(enum_spec)

    def visit_enumeration(self, enum_spec: LspSpec) -> None:
        enum_name: str = enum_spec["name"]
        if self.name_index.get(enum_name, None) != enum_name:
            enum_kind = LspSymbolKind.ENUMERATION
            enum_symbol = LspSymbol(enum_name, enum_kind, enum_spec)
            self.index_by_name(enum_name, enum_symbol)
            self.index_by_kind(enum_kind, enum_symbol)
            BaseLspSpecVisitor.visit_enumeration(self, enum_spec)

    def visit_structures(self) -> None:
        for struct_spec in self.schema["structures"]:
            self.visit_structure(struct_spec)

    def visit_structure(self, struct_spec: LspSpec) -> None:
        struct_name: str = struct_spec["name"]
        if self.name_index.get(struct_name, None) != struct_name:
            struct_kind = LspSymbolKind.STRUCTURE
            struct_symbol = LspSymbol(struct_name, struct_kind, struct_spec)
            self.index_by_name(struct_name, struct_symbol)
            self.index_by_kind(struct_kind, struct_symbol)
            BaseLspSpecVisitor.visit_structure(self, struct_spec)

    def visit_base(self, base_spec: LspSpec) -> None:
        base_name: str = base_spec["name"]
        if self.name_index.get(base_name, None) != base_name:
            base_kind = LspSymbolKind.BASE
            base_symbol = LspSymbol(base_name, base_kind, base_spec)
            self.index_by_name(base_name, base_symbol)
            self.index_by_kind(base_kind, base_symbol)
            BaseLspSpecVisitor.visit_base(self, base_spec)

    def share_cycles(self, child_symbol: LspSymbol) -> None:
        if len(self.nested_names) > 1:
            parent_name = self.nested_name(list(self.nested_names)[:-1])
            parent_symbol = self.name_index[parent_name]
            if parent_symbol.cycles is None:
                parent_symbol.cycles = set()
            child_symbol.cycles = parent_symbol.cycles

    def visit_array(self, array_spec: LspSpec) -> None:
        array_name = self.nested_name()
        if array_name not in self.name_index:
            array_kind = LspSymbolKind.ARRAY
            array_symbol = LspSymbol(array_name, array_kind, array_spec)
            self.index_by_name(array_name, array_symbol)
            self.index_by_kind(array_kind, array_symbol)
            BaseLspSpecVisitor.visit_array(self, array_spec)
            self.share_cycles(array_symbol)

    def visit_map(self, map_spec: LspSpec) -> None:
        map_name = self.nested_name()
        if map_name not in self.name_index:
            map_kind = LspSymbolKind.MAP
            map_symbol = LspSymbol(map_name, map_kind, map_spec)
            self.index_by_name(map_name, map_symbol)
            self.index_by_kind(map_kind, map_symbol)
            BaseLspSpecVisitor.visit_map(self, map_spec)
            self.share_cycles(map_symbol)

    def visit_tuple(self, tuple_spec: LspSpec) -> None:
        tuple_name = self.nested_name()
        if tuple_name not in self.name_index:
            tuple_kind = LspSymbolKind.TUPLE
            tuple_symbol = LspSymbol(tuple_name, tuple_kind, tuple_spec)
            self.index_by_name(tuple_name, tuple_symbol)
            self.index_by_kind(tuple_kind, tuple_symbol)
            BaseLspSpecVisitor.visit_tuple(self, tuple_spec)

    def visit_union(self, union_spec: LspSpec) -> None:
        union_name: str = self.nested_name()
        if self.name_index.get(union_name, None) != union_name:
            union_kind = LspSymbolKind.UNION
            union_symbol = LspSymbol(union_name, union_kind, union_spec)
            self.index_by_name(union_name, union_symbol)
            self.index_by_kind(union_kind, union_symbol)
            BaseLspSpecVisitor.visit_union(self, union_spec)

    def visit_inner(self, inner_spec: LspSpec) -> None:
        nested_name: str = self.nested_name()
        if self.name_index.get(nested_name, None) != nested_name:
            nested_kind = LspSymbolKind.INNER
            inner_symbol = LspSymbol(nested_name, nested_kind, inner_spec)
            self.index_by_name(nested_name, inner_symbol)
            self.index_by_kind(nested_kind, inner_symbol)
            BaseLspSpecVisitor.visit_inner(self, inner_spec)

    def visit_type_aliases(self) -> None:
        for alias_spec in self.schema["typeAliases"]:
            self.visit_type_alias(alias_spec)

    def visit_type_alias(self, alias_spec: LspSpec) -> None:
        BaseLspSpecVisitor.visit_type_alias(self, alias_spec)
        alias_name: str = alias_spec["name"]
        if alias_name not in self.name_index:
            alias_kind = LspSymbolKind.ALIAS
            alias_symbol = LspSymbol(alias_name, alias_kind, alias_spec)
            self.index_by_name(alias_name, alias_symbol)
            self.index_by_kind(alias_kind, alias_symbol)

    def visit_requests(self) -> None:
        for request_spec in self.schema["requests"]:
            self.visit_request(request_spec)

    def visit_request(self, request_spec: LspSpec) -> None:
        request_method = request_spec["method"]
        if request_method not in self.name_index:
            request_kind = LspSymbolKind.REQUEST
            request_symbol = LspSymbol(request_method, request_kind, request_spec)
            self.index_by_name(request_method, request_symbol)
            self.index_by_kind(request_kind, request_symbol)
            request_direction = request_spec["messageDirection"]
            self.index_request_by_direction(request_direction, request_symbol)
            BaseLspSpecVisitor.visit_request(self, request_spec)

    def visit_notifications(self) -> None:
        for notification_spec in self.schema["notifications"]:
            self.visit_notification(notification_spec)

    def visit_notification(self, notification_spec: LspSpec) -> None:
        notification_method = notification_spec["method"]
        if notification_method not in self.name_index:
            notification_kind = LspSymbolKind.NOTIFICATION
            notification_symbol = LspSymbol(notification_method, notification_kind, notification_spec)
            self.index_by_name(notification_method, notification_symbol)
            self.index_by_kind(notification_kind, notification_symbol)
            notification_direction = notification_spec["messageDirection"]
            self.index_notification_by_direction(notification_direction, notification_symbol)
            BaseLspSpecVisitor.visit_notification(self, notification_spec)


class LspSymbolVisitor(ABC, Generic[T]):

    def visit_symbol(self, symbol: LspSymbol) -> T:
        match symbol.kind:
            case LspSymbolKind.ALIAS:
                return self.visit_alias_symbol(symbol)
            case LspSymbolKind.ARRAY:
                return self.visit_array_symbol(symbol)
            case LspSymbolKind.BASE:
                return self.visit_base_symbol(symbol)
            case LspSymbolKind.ENUMERATION:
                return self.visit_enumeration_symbol(symbol)
            case LspSymbolKind.INNER:
                return self.visit_inner_symbol(symbol)
            case LspSymbolKind.MAP:
                return self.visit_map_symbol(symbol)
            case LspSymbolKind.NOTIFICATION:
                return self.visit_notification_symbol(symbol)
            case LspSymbolKind.REFERENCE:
                return self.visit_reference_symbol(symbol)
            case LspSymbolKind.REQUEST:
                return self.visit_request_symbol(symbol)
            case LspSymbolKind.STRUCTURE:
                return self.visit_structure_symbol(symbol)
            case LspSymbolKind.TUPLE:
                return self.visit_tuple_symbol(symbol)
            case LspSymbolKind.UNION:
                return self.visit_union_symbol(symbol)

    @abstractmethod
    def visit_alias_symbol(self, alias_symbol: LspSymbol) -> T:
        raise NotImplementedError

    @abstractmethod
    def visit_array_symbol(self, array_symbol: LspSymbol) -> T:
        raise NotImplementedError

    @abstractmethod
    def visit_base_symbol(self, base_symbol: LspSymbol) -> T:
        raise NotImplementedError

    @abstractmethod
    def visit_enumeration_symbol(self, enum_symbol: LspSymbol) -> T:
        raise NotImplementedError

    @abstractmethod
    def visit_inner_symbol(self, inner_symbol: LspSymbol) -> T:
        raise NotImplementedError

    @abstractmethod
    def visit_map_symbol(self, map_symbol: LspSymbol) -> T:
        raise NotImplementedError

    @abstractmethod
    def visit_notification_symbol(self, notification_symbol: LspSymbol) -> T:
        raise NotImplementedError

    @abstractmethod
    def visit_reference_symbol(self, reference_symbol: LspSymbol) -> T:
        raise NotImplementedError

    @abstractmethod
    def visit_request_symbol(self, request_symbol: LspSymbol) -> T:
        raise NotImplementedError

    @abstractmethod
    def visit_structure_symbol(self, struct_symbol: LspSymbol) -> T:
        raise NotImplementedError

    @abstractmethod
    def visit_tuple_symbol(self, tuple_symbol: LspSymbol) -> T:
        raise NotImplementedError

    @abstractmethod
    def visit_union_symbol(self, union_symbol: LspSymbol) -> T:
        raise NotImplementedError


class BaseLspSymbolVisitor(LspSymbolVisitor[None], BaseLspSpecVisitor):
    indexer: LspIndexer

    def __init__(self, indexer: LspIndexer) -> None:
        LspSymbolVisitor.__init__(self)
        BaseLspSpecVisitor.__init__(self)
        self.indexer = indexer

    def visit_reference(self, reference_spec: LspSpec) -> None:
        reference_name: str = reference_spec["name"]
        with self.nested_name_as(reference_name):
            reference_symbol = self.indexer.name_index[reference_name]
            self.visit_symbol(reference_symbol)
            BaseLspSpecVisitor.visit_reference(self, reference_spec)

    def visit_enumeration(self, enum_spec: LspSpec) -> None:
        enum_name: str = enum_spec["name"]
        enum_symbol = self.indexer.name_index[enum_name]
        self.visit_symbol(enum_symbol)
        BaseLspSpecVisitor.visit_enumeration(self, enum_spec)

    def visit_structure(self, struct_spec: LspSpec) -> None:
        struct_name: str = struct_spec["name"]
        struct_symbol = self.indexer.name_index[struct_name]
        self.visit_symbol(struct_symbol)
        BaseLspSpecVisitor.visit_structure(self, struct_spec)

    def visit_base(self, base_spec: LspSpec) -> None:
        base_name: str = base_spec["name"]
        base_symbol = self.indexer.name_index[base_name]
        self.visit_symbol(base_symbol)
        BaseLspSpecVisitor.visit_base(self, base_spec)

    def visit_union(self, union_spec: LspSpec) -> None:
        union_name: str = self.nested_name()
        union_symbol = self.indexer.name_index[union_name]
        self.visit_symbol(union_symbol)
        BaseLspSpecVisitor.visit_union(self, union_spec)

    def visit_inner(self, inner_spec: LspSpec) -> None:
        nested_name: str = self.nested_name()
        inner_symbol = self.indexer.name_index[nested_name]
        self.visit_symbol(inner_symbol)
        BaseLspSpecVisitor.visit_inner(self, inner_spec)

    def visit_type_alias(self, alias_spec: LspSpec) -> None:
        alias_name: str = alias_spec["name"]
        alias_symbol = self.indexer.name_index[alias_name]
        self.visit_symbol(alias_symbol)
        BaseLspSpecVisitor.visit_type_alias(self, alias_spec)

    def visit_request(self, request_spec: LspSpec) -> None:
        request_method = request_spec["method"]
        request_symbol = self.indexer.name_index[request_method]
        self.visit_symbol(request_symbol)
        BaseLspSpecVisitor.visit_request(self, request_spec)

    def visit_notification(self, notification_spec: LspSpec) -> None:
        notification_method = notification_spec["method"]
        notification_symbol = self.indexer.name_index[notification_method]
        self.visit_symbol(notification_symbol)
        BaseLspSpecVisitor.visit_notification(self, notification_spec)

    def visit_alias_symbol(self, alias_symbol: LspSymbol) -> None:
        alias_spec = alias_symbol.spec
        self.visit_type_alias(alias_spec)

    def visit_array_symbol(self, array_symbol: LspSymbol) -> None:
        array_spec = array_symbol.spec
        self.visit_array(array_spec)

    def visit_base_symbol(self, base_symbol: LspSymbol) -> None:
        base_spec = base_symbol.spec
        self.visit_base(base_spec)

    def visit_enumeration_symbol(self, enum_symbol: LspSymbol) -> None:
        enum_spec = enum_symbol.spec
        self.visit_enumeration(enum_spec)

    def visit_inner_symbol(self, inner_symbol: LspSymbol) -> None:
        inner_spec = inner_symbol.spec
        self.visit_inner(inner_spec)

    def visit_map_symbol(self, map_symbol: LspSymbol) -> None:
        map_spec = map_symbol.spec
        self.visit_map(map_spec)

    def visit_notification_symbol(self, notification_symbol: LspSymbol) -> None:
        notification_spec = notification_symbol.spec
        self.visit_notification(notification_spec)

    def visit_reference_symbol(self, reference_symbol: LspSymbol) -> None:
        reference_spec = reference_symbol.spec
        self.visit_reference(reference_spec)

    def visit_request_symbol(self, request_symbol: LspSymbol) -> None:
        request_spec = request_symbol.spec
        self.visit_request(request_spec)

    def visit_structure_symbol(self, struct_symbol: LspSymbol) -> None:
        struct_spec = struct_symbol.spec
        self.visit_structure(struct_spec)

    def visit_tuple_symbol(self, tuple_symbol: LspSymbol) -> None:
        tuple_spec = tuple_symbol.spec
        self.visit_tuple(tuple_spec)

    def visit_union_symbol(self, union_symbol: LspSymbol) -> None:
        union_spec = union_symbol.spec
        self.visit_union(union_spec)


def dependency_visitor(fn: Callable) -> Callable:
    @wraps(fn)
    def wrapper(self, symbol: LspSymbol) -> None:
        if self.dependencies is not None \
           and symbol not in self.dependencies \
           and symbol not in self.blacklist:
            if symbol is self.context \
               and symbol not in self.cyclic_dependencies:
                self.cyclic_dependencies[symbol] = True
            self.dependencies[symbol] = True
            if symbol.dependencies is not None:
                # Use the cached set of dependencies
                for dependency in symbol.dependencies:
                    if dependency is self.context \
                       and dependency not in self.cyclic_dependencies:
                        self.cyclic_dependencies[dependency] = True
                    self.dependencies[dependency] = True
            else:
                fn(self, symbol)
    return wrapper


class LspDependencyExtractor(BaseLspSymbolVisitor):
    cyclic_dependencies: Dict[LspSymbol, bool]
    blacklist: Set[LspSymbol]
    dependencies: Optional[Dict[LspSymbol, bool]]
    context: Optional[LspSymbol]

    def __init__(self, indexer: LspIndexer) -> None:
        BaseLspSymbolVisitor.__init__(self, indexer)
        self.cyclic_dependencies = OrderedDict()
        self.blacklist = set()
        self.dependencies = None
        self.context = None

    @contextmanager
    def push_context(self, context: LspSymbol) -> Iterator[Dict[LspSymbol, bool]]:
        old_dependencies = self.dependencies
        old_context = self.context

        self.dependencies = OrderedDict()
        self.context = context

        yield self.dependencies

        self.dependencies = old_dependencies
        self.context = old_context

    def extract(self) -> None:
        for symbol in self.indexer.name_index.values():
            self.visit_symbol(symbol)
            if symbol in self.cyclic_dependencies \
               and symbol.dependencies is not None:
                if symbol.cycles is None:
                    symbol.cycles = set()
                for dependency in symbol.dependencies:
                    if dependency.dependencies is not None \
                       and symbol in dependency.dependencies:
                        symbol.cycles.add(dependency)
                        if dependency.cycles is None:
                            dependency.cycles = set()
                        dependency.cycles.add(symbol)

    def visit_symbol(self, symbol: LspSymbol) -> None:
        with self.nested_name_as(symbol.name) as symbol_name:
            if symbol.dependencies is None:
                symbol.dependencies = []  # avoid infinite recursion on cyclic dependencies
                with self.push_context(symbol) as dependencies:
                    match symbol.kind:
                        case LspSymbolKind.ALIAS:
                            BaseLspSpecVisitor.visit_type_alias(self, symbol.spec)
                        case LspSymbolKind.ARRAY:
                            BaseLspSpecVisitor.visit_array(self, symbol.spec)
                        case LspSymbolKind.BASE:
                            BaseLspSpecVisitor.visit_base(self, symbol.spec)
                        case LspSymbolKind.ENUMERATION:
                            BaseLspSpecVisitor.visit_enumeration(self, symbol.spec)
                        case LspSymbolKind.INNER:
                            BaseLspSpecVisitor.visit_inner(self, symbol.spec)
                        case LspSymbolKind.MAP:
                            BaseLspSpecVisitor.visit_map(self, symbol.spec)
                        case LspSymbolKind.NOTIFICATION:
                            BaseLspSpecVisitor.visit_notification(self, symbol.spec)
                        case LspSymbolKind.REFERENCE:
                            BaseLspSpecVisitor.visit_reference(self, symbol.spec)
                        case LspSymbolKind.REQUEST:
                            BaseLspSpecVisitor.visit_request(self, symbol.spec)
                        case LspSymbolKind.STRUCTURE:
                            BaseLspSpecVisitor.visit_structure(self, symbol.spec)
                        case LspSymbolKind.TUPLE:
                            BaseLspSpecVisitor.visit_tuple(self, symbol.spec)
                        case LspSymbolKind.UNION:
                            BaseLspSpecVisitor.visit_union(self, symbol.spec)
                    symbol.dependencies.extend(dependencies.keys())
            BaseLspSymbolVisitor.visit_symbol(self, symbol)

    @dependency_visitor
    def visit_alias_symbol(self, alias_symbol: LspSymbol) -> None:
        BaseLspSymbolVisitor.visit_alias_symbol(self, alias_symbol)

    @dependency_visitor
    def visit_array_symbol(self, array_symbol: LspSymbol) -> None:
        BaseLspSymbolVisitor.visit_array_symbol(self, array_symbol)

    @dependency_visitor
    def visit_base_symbol(self, base_symbol: LspSymbol) -> None:
        BaseLspSymbolVisitor.visit_base_symbol(self, base_symbol)

    @dependency_visitor
    def visit_enumeration_symbol(self, enum_symbol: LspSymbol) -> None:
        BaseLspSymbolVisitor.visit_enumeration_symbol(self, enum_symbol)

    @dependency_visitor
    def visit_inner_symbol(self, inner_symbol: LspSymbol) -> None:
        BaseLspSymbolVisitor.visit_inner_symbol(self, inner_symbol)

    @dependency_visitor
    def visit_map_symbol(self, map_symbol: LspSymbol) -> None:
        BaseLspSymbolVisitor.visit_map_symbol(self, map_symbol)

    @dependency_visitor
    def visit_notification_symbol(self, notification_symbol: LspSymbol) -> None:
        BaseLspSymbolVisitor.visit_notification_symbol(self, notification_symbol)

    @dependency_visitor
    def visit_reference_symbol(self, reference_symbol: LspSymbol) -> None:
        BaseLspSymbolVisitor.visit_reference_symbol(self, reference_symbol)

    @dependency_visitor
    def visit_request_symbol(self, request_symbol: LspSymbol) -> None:
        BaseLspSymbolVisitor.visit_request_symbol(self, request_symbol)

    @dependency_visitor
    def visit_structure_symbol(self, struct_symbol: LspSymbol) -> None:
        BaseLspSymbolVisitor.visit_structure_symbol(self, struct_symbol)

    @dependency_visitor
    def visit_tuple_symbol(self, tuple_symbol: LspSymbol) -> None:
        BaseLspSymbolVisitor.visit_tuple_symbol(self, tuple_symbol)

    @dependency_visitor
    def visit_union_symbol(self, union_symbol: LspSymbol) -> None:
        BaseLspSymbolVisitor.visit_union_symbol(self, union_symbol)


class LspSymbolResolver(LspSpecVisitor[LspSymbol], NestedContext):
    indexer: LspIndexer

    def __init__(self, indexer: LspIndexer) -> None:
        LspSpecVisitor.__init__(self)
        NestedContext.__init__(self)
        self.indexer = indexer

    def resolve_all(self) -> None:
        for symbol in self.indexer.name_index.values():
            self.resolve(symbol)

    def resolve(self, symbol_or_name: Optional[Union[str, LspSymbol]] = None) -> LspSymbol:
        match symbol_or_name:
            case str():
                symbol_name: str = symbol_or_name
                symbol: LspSymbol = self.indexer.name_index[symbol_name]
            case LspSymbol():
                symbol: LspSymbol = symbol_or_name
            case _:
                raise ValueError(f'Unsupported symbol type ({type(symbol_or_name)}): {symbol_or_name}')
        if symbol.resolution is None:
            match symbol.kind:
                case LspSymbolKind.ALIAS:
                    symbol.resolution = self.visit_alias_symbol(symbol)
                case LspSymbolKind.REFERENCE:
                    symbol.resolution = self.visit_reference_symbol(symbol)
                case _:
                    symbol.resolution = symbol
        return symbol.resolution

    def visit_alias_symbol(self, alias_symbol: LspSymbol) -> LspSymbol:
        alias_spec = alias_symbol.spec
        return self.visit_type_alias(alias_spec)

    def visit_reference_symbol(self, reference_symbol: LspSymbol) -> LspSymbol:
        reference_spec = reference_symbol.spec
        return self.visit_type(reference_spec)

    def visit_base(self, base_spec: LspSpec) -> LspSymbol:
        base_name = base_spec["name"]
        return self.resolve(base_name)

    def visit_reference(self, reference_spec: LspSpec) -> LspSymbol:
        type_name = reference_spec["name"]
        return self.resolve(type_name)

    def visit_enumeration(self, enum_spec: LspSpec) -> LspSymbol:
        enum_name = enum_spec["name"]
        return self.resolve(enum_name)

    def visit_array(self, array_spec: LspSpec) -> LspSymbol:
        array_name = self.nested_name()
        return self.resolve(array_name)

    def visit_map(self, map_spec: LspSpec) -> LspSymbol:
        map_name = self.nested_name()
        return self.resolve(map_name)

    def visit_and(self, and_spec: LspSpec) -> LspSymbol:
        and_name = self.nested_name()
        return self.resolve(and_name)

    def visit_union(self, union_spec: LspSpec) -> LspSymbol:
        union_name = self.nested_name()
        return self.resolve(union_name)

    def visit_tuple(self, tuple_spec: LspSpec) -> LspSymbol:
        tuple_name = self.nested_name()
        return self.resolve(tuple_name)

    def visit_inner(self, inner_spec: LspSpec) -> LspSymbol:
        nested_name = self.nested_name()
        return self.resolve(nested_name)

    def visit_structure(self, struct_spec: LspSpec) -> LspSymbol:
        struct_name: str = struct_spec["name"]
        return self.resolve(struct_name)

    def visit_type_alias(self, alias_spec: LspSpec) -> LspSymbol:
        alias_name = alias_spec["name"]
        with self.nested_name_as(alias_name):
            type_spec: LspSpec = alias_spec["type"]
            return self.visit_type(type_spec)

    def visit_request(self, request_spec: LspSpec) -> LspSymbol:
        request_method = request_spec["method"]
        return self.resolve(request_method)

    def visit_notification(self, notification_spec: LspSpec) -> LspSymbol:
        notification_method = notification_spec["method"]
        return self.resolve(notification_method)

    def visit_string_literal(self, string_spec: LspSpec) -> LspSymbol:
        raise NotImplementedError

    def visit_integer_literal(self, integer_spec: LspSpec) -> LspSymbol:
        raise NotImplementedError

    def visit_boolean_literal(self, boolean_spec: LspSpec) -> LspSymbol:
        raise NotImplementedError


class LspStructFieldExpander:
    indexer: LspIndexer
    resolver: LspSymbolResolver

    def __init__(self, indexer: LspIndexer) -> None:
        self.indexer = indexer
        self.resolver = LspSymbolResolver(indexer)

    def expand(self) -> None:
        for struct_symbol in self.indexer.kind_index[LspSymbolKind.STRUCTURE]:
            self.visit_structure_symbol(struct_symbol)
        for inner_symbol in self.indexer.kind_index[LspSymbolKind.INNER]:
            self.visit_inner_symbol(inner_symbol)

    def visit_structure_symbol(self, struct_symbol: LspSymbol) -> None:
        struct_fields = chain()
        pending: Deque[LspSymbol] = deque([struct_symbol])
        while len(pending) > 0:
            pending_symbol = pending.popleft()
            pending_spec = pending_symbol.spec
            struct_fields = chain(
                pending_spec["properties"],
                struct_fields
            )
            mixin_specs = pending_spec.get("mixins", None)
            if mixin_specs is not None:
                for mixin_spec in mixin_specs:
                    mixin_symbol = self.resolver.resolve(mixin_spec["name"])
                    pending.append(mixin_symbol)
        struct_symbol.fields = list(struct_fields)

    def visit_inner_symbol(self, inner_symbol: LspSymbol) -> None:
        inner_spec = inner_symbol.spec
        value_spec: LspSpec = inner_spec["value"]
        inner_symbol.fields = list(value_spec["properties"])


class LspAnalysisPipeline:
    schema: LspSpec
    indexer: LspIndexer
    dependency_extractor: LspDependencyExtractor
    struct_field_expander: LspStructFieldExpander
    symbol_resolver: LspSymbolResolver

    def __init__(self, schema: LspSpec) -> None:
        self.schema = schema
        self.indexer = LspIndexer(schema)
        self.dependency_extractor = LspDependencyExtractor(self.indexer)
        self.struct_field_expander = LspStructFieldExpander(self.indexer)
        self.symbol_resolver = LspSymbolResolver(self.indexer)

    def run(self) -> None:
        self.indexer.index()
        self.dependency_extractor.extract()
        self.struct_field_expander.expand()
        self.symbol_resolver.resolve_all()


class LspTopologicalIterator(Iterable[LspSymbol]):
    dependencies_by_symbol: Dict[LspSymbol, List[LspSymbol]]
    dependents_by_symbol: Dict[LspSymbol, List[LspSymbol]]

    def __init__(self, indexer: LspIndexer) -> None:
        self.dependencies_by_symbol = OrderedDict()
        self.dependents_by_symbol = OrderedDict()

        for symbol in indexer.name_index.values():
            if symbol.dependencies is not None:
                dependencies = []
                for dependency in symbol.dependencies:
                    dependencies.append(dependency)
                    dependents = self.dependents_by_symbol.get(dependency, None)
                    if dependents is None:
                        dependents = []
                        self.dependents_by_symbol[dependency] = dependents
                    dependents.append(symbol)
                self.dependencies_by_symbol[symbol] = dependencies
            else:
                raise ValueError(
                    f'No dependencies were defined for symbol: {symbol.name}'
                )

    def __iter__(self) -> Iterator[LspSymbol]:
        yield from self.sort()

    def resolve(
            self,
            resolved: Set[LspSymbol],
            symbol: LspSymbol
    ) -> Iterator[LspSymbol]:
        if symbol not in resolved:
            yield symbol
            resolved.add(symbol)
            dependents = self.dependents_by_symbol.get(symbol, None)
            if dependents is not None:
                for dependent in self.dependents_by_symbol[symbol]:
                    dependent_dependencies = \
                        self.dependencies_by_symbol.get(dependent, None)
                    if dependent_dependencies is not None:
                        if symbol in dependent_dependencies:
                            dependent_dependencies.remove(symbol)
                        if len(dependent_dependencies) == 0:
                            yield from self.resolve(resolved, dependent)
                del self.dependents_by_symbol[symbol]
            if symbol in self.dependencies_by_symbol:
                del self.dependencies_by_symbol[symbol]

    def sort(self) -> Iterator[LspSymbol]:
        resolved: Set[LspSymbol] = set()
        for symbol, dependencies in list(self.dependencies_by_symbol.items()):
            if len(dependencies) == 0:
                yield from self.resolve(resolved, symbol)
        num_failed = len(self.dependencies_by_symbol)
        if num_failed > 0:
            raise RuntimeError(f'Failed to sort {num_failed} dependencies.')
