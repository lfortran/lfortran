from collections import deque
from itertools import chain
from pathlib import Path
from typing import Any, Callable, Deque, Dict, List, Set, Tuple, Union

from llanguage_server.auxiliary_schema import AUXILIARY_SCHEMA
from llanguage_server.cxx.file_generator import CPlusPlusLspFileGenerator
from llanguage_server.utils import (lower_first, method_to_camel_case,
                                    method_to_underscore, rename_enum,
                                    rename_type)


class CPlusPlusSpecificationHeaderGenerator(CPlusPlusLspFileGenerator):
    generated: Set[str]
    generators: Dict[str, Callable[[Dict[str, Any]], None]]
    dependencies: Dict[str, Set[str]]
    dependents: Dict[str, Set[str]]

    def __init__(
        self,
        output_dir: Path,
        schema: Dict[str, Any],
        namespace: str,
        symbols: Dict[str, Tuple[str, Dict[str, Any]]]
    ) -> None:
        specification_header = output_dir / "specification.h"
        super().__init__(specification_header, schema, namespace, symbols)
        self.generated = set()
        self.generators = {}
        self.dependencies = {}
        self.dependents = {}

    def extract_string_dependencies(
        self,
        dependencies: Set[str],
        base_spec: Dict[str, Any]
    ) -> None:
        type_name = "string"
        if type_name not in self.generated:
            raise ValueError(f'Base type not generated: {type_name}')

    def extract_integer_dependencies(
        self,
        dependencies: Set[str],
        base_spec: Dict[str, Any]
    ) -> None:
        type_name = "integer"
        if type_name not in self.generated:
            raise ValueError(f'Base type not generated: {type_name}')

    def extract_boolean_dependencies(
        self,
        dependencies: Set[str],
        base_spec: Dict[str, Any]
    ) -> None:
        type_name = "boolean"
        if type_name not in self.generated:
            raise ValueError(f'Base type not generated: {type_name}')

    def extract_base_dependencies(
        self,
        dependencies: Set[str],
        base_spec: Dict[str, Any]
    ) -> None:
        type_name = base_spec["name"]
        if type_name not in self.generated:
            raise ValueError(f'Base type not generated: {type_name}')

    def extract_reference_dependencies(
        self,
        dependencies: Set[str],
        ref_spec: Dict[str, Any]
    ) -> None:
        type_name = ref_spec["name"]
        if type_name not in self.generated:
            dependencies.add(type_name)

    def extract_array_dependencies(
        self,
        dependencies: Set[str],
        array_spec: Dict[str, Any]
    ) -> None:
        self.extract_type_dependencies(dependencies, array_spec["element"])

    def extract_tuple_dependencies(
        self,
        dependencies: Set[str],
        tuple_spec: Dict[str, Any]
    ) -> None:
        item_specs = tuple_spec["items"]
        for item_spec in item_specs:
            self.extract_type_dependencies(dependencies, item_spec)

    def extract_map_dependencies(
        self,
        dependencies: Set[str],
        map_spec: Dict[str, Any]
    ) -> None:
        key_spec = map_spec["key"]
        match key_spec["kind"]:
            case "base":
                self.extract_base_dependencies(dependencies, key_spec)
            case "reference":
                self.extract_reference_dependencies(dependencies, key_spec)
            case _:
                raise ValueError(f'Unsupported map-key type: {map_key}')
        value_spec = map_spec["value"]
        self.extract_type_dependencies(dependencies, value_spec)

    def extract_and_dependencies(
        self,
        dependencies: Set[str],
        and_spec: Dict[str, Any]
    ) -> None:
        for item_spec in and_spec["items"]:
            self.extract_type_dependencies(dependencies, item_spec)

    def extract_or_dependencies(
        self,
        dependencies: Set[str],
        or_spec: Dict[str, Any]
    ) -> None:
        for item_spec in or_spec["items"]:
            self.extract_type_dependencies(dependencies, item_spec)

    def extract_structure_literal_dependencies(
        self,
        dependencies: Set[str],
        nested_spec: Dict[str, Any]
    ) -> None:
        for prop_spec in nested_spec["value"]["properties"]:
            self.extract_property_dependencies(dependencies, prop_spec)

    def extract_type_dependencies(
        self,
        dependencies: Set[str],
        type_spec: Dict[str, Any]
    ) -> None:
        match type_spec["kind"]:
            case "base":
                self.extract_base_dependencies(dependencies, type_spec)
            case "reference":
                self.extract_reference_dependencies(dependencies, type_spec)
            case "array":
                self.extract_array_dependencies(dependencies, type_spec)
            case "map":
                self.extract_map_dependencies(dependencies, type_spec)
            case "and":
                self.extract_and_dependencies(dependencies, type_spec)
            case "or":
                self.extract_or_dependencies(dependencies, type_spec)
            case "tuple":
                self.extract_tuple_dependencies(dependencies, type_spec)
            case "literal":
                self.extract_structure_literal_dependencies(dependencies, type_spec)
            case "stringLiteral":
                self.extract_string_dependencies(dependencies, type_spec)
            case "integerLiteral":
                self.extract_integer_dependencies(dependencies, type_spec)
            case "booleanLiteral":
                self.extract_boolean_dependencies(dependencies, type_spec)
            case _:
                raise ValueError(f'Unsupported Type kind: {type_spec}')

    def extract_property_dependencies(
        self,
        dependencies: Set[str],
        prop_spec: Dict[str, Any]
    ) -> None:
        type_spec = prop_spec["type"]
        self.extract_type_dependencies(dependencies, type_spec)

    def add_pending(
        self,
        name: str,
        dependencies: Set[str],
        generate_fn: Callable[[Dict[str, Any]], None]
    ) -> None:
        self.dependencies[name] = dependencies
        self.generators[name] = generate_fn
        for dependency in dependencies:
            dependency_dependents = self.dependents.get(dependency, None)
            if dependency_dependents is None:
                dependency_dependents = set()
                self.dependents[dependency] = dependency_dependents
            dependency_dependents.add(name)

    def add_generated(self, name: str) -> None:
        if name not in self.generated:
            self.generated.add(name)
            dependents = self.dependents.get(name, None)
            if dependents is not None:
                for dependent_name in dependents:
                    dependent_dependencies = self.dependencies[dependent_name]
                    # If `name` is not in `dependent_dependencies` then it was part of a
                    # cycle.
                    if name in dependent_dependencies:
                        dependent_dependencies.remove(name)
                    if len(dependent_dependencies) == 0:
                        generate_dependent = self.generators[dependent_name]
                        _, dependent = self.symbols[dependent_name]
                        generate_dependent(dependent)
                        self.add_generated(dependent_name)
                        del self.dependencies[dependent_name]

    def generate_or_add_to_pending(
        self,
        name: str,
        subject: Dict[str, Any],
        dependencies: Set[str],
        generate_fn: Callable[[Dict[str, Any]], None]
    ) -> bool:
        if len(dependencies) == 0:
            generate_fn(subject)
            self.add_generated(name)
            return True
        self.add_pending(name, dependencies, generate_fn)
        return False

    def resolve_cycles(self) -> None:
        pending: Deque[Tuple[str, Set[str]]] = deque(self.dependencies.items())
        closures: Dict[str, Deque[str]] = {}
        while len(pending) > 0:
            name, dependencies = pending.pop()
            closure = closures.get(name, None)
            if closure is None:
                closure = deque()
                closures[name] = closure
            for dependency_name in dependencies:
                if dependency_name not in closure:
                    closure.append(dependency_name)
                    dependency_dependencies = self.dependencies[dependency_name]
                    pending.append((dependency_name, dependency_dependencies))
        declared: Set[str] = set()
        for name, closure in closures.items():
            while len(closure) > 0:
                dependency_name = closure.popleft()
                if dependency_name not in self.generated \
                     and dependency_name not in declared:
                    self.write(f'struct {dependency_name};    // forward declaration')
                    self.newline()
                    declared.add(dependency_name)
            self.add_generated(name)

    def generate_enumeration(self, enum_spec: Dict[str, Any]) -> None:
        enum_name = enum_spec["name"]
        lower_name = lower_first(enum_name)
        type_spec = enum_spec["type"]
        type_name = type_spec["name"]
        value_type = rename_type(type_name)
        self.generate_docstring(enum_spec.get("documentation", None))
        self.write(f'enum class {enum_name} {{')
        with self.indent():
            if type_name == "string":
                for value in enum_spec["values"]:
                    self.generate_docstring(value.get("documentation", None))
                    self.write(f'{rename_enum(value["name"])},')
            else:
                for value in enum_spec["values"]:
                    self.generate_docstring(value.get("documentation", None))
                    self.write(f'{rename_enum(value["name"])} = {value["value"]},')
        self.write('};')
        self.newline()
        self.write('extern std::map<')
        with self.indent(): self.write(f'{enum_name},')
        with self.indent(): self.write('std::string')
        self.write(f'> {enum_name}Names;')
        self.newline()
        if type_name == "string":
            self.write('extern std::map<')
            with self.indent(): self.write(f'{enum_name},')
            with self.indent(): self.write(value_type)
            self.write(f'> {enum_name}Values;')
            self.newline()
        self.write(f'auto {lower_name}ByName(')
        with self.indent(): self.write('const std::string &name')
        self.write(f') -> {enum_name};')
        self.newline()
        if type_name == "string":
            self.write(f'auto {lower_name}ByValue(')
            with self.indent(): self.write(f'const {value_type} &value')
            self.write(f') -> {enum_name};')
        else:
            self.write(f'auto {lower_name}ByValue(')
            with self.indent(): self.write(f'{value_type} value')
            self.write(f') -> {enum_name};')

    def generate_enumerations(self) -> None:
        enum_specs = chain(
            AUXILIARY_SCHEMA["enumerations"],
            self.schema["enumerations"],
        )
        for enum_spec in enum_specs:
            enum_name = enum_spec["name"]
            self.symbols[enum_name] = ("enumeration", enum_spec)
            self.generate_enumeration(enum_spec)
            self.newline()
            self.generated.add(enum_name)

    def generate_property(self, prop_spec: Dict[str, Any]) -> None:
        with self.nest_name(prop_spec["name"]):
            type_spec = prop_spec["type"]
            optional = prop_spec.get("optional", False)
            if optional:
                self.inline('std::optional<')
            self.generate_type_declaration(prop_spec["type"])
            if optional:
                self.inline('>')
            self.inline(f' {prop_spec["name"]};', end='\n')

    def extract_nested_dependencies(
        self,
        nested_dependencies: Deque[
            Tuple[
                Deque[str],
                Callable[
                    [
                        Deque[str],
                        Dict[str, Any]
                    ],
                    None
                ],
                Dict[str, Any]
            ]
        ],
        spec: Dict[str, Any]
    ) -> None:
        match spec["kind"]:
            case "base" | "stringLiteral" | "integerLiteral" | "booleanLiteral" | "reference":
                pass
            case "array":
                self.extract_nested_dependencies(nested_dependencies, spec["element"])
            case "map":
                self.extract_nested_dependencies(nested_dependencies, spec["key"])
                self.extract_nested_dependencies(nested_dependencies, spec["value"])
            case "and":
                raise ValueError(
                    f'AND types are not supported for type declarations: {spec}'
                )
            case "or":
                nested_dependencies.append((
                    deque(self.nested_names),
                    self.generate_nested_variant,
                    spec
                ))
                for index, item_spec in enumerate(spec["items"]):
                    with self.nest_name(str(index)):
                        self.extract_nested_dependencies(nested_dependencies, item_spec)
            case "tuple":
                for item_spec in spec["items"]:
                    self.extract_nested_dependencies(nested_dependencies, item_spec)
            case "literal":
                nested_dependencies.append((
                    deque(self.nested_names),
                    self.generate_nested_structure,
                    spec
                ))
                for prop_spec in spec["value"]["properties"]:
                    with self.nest_name(prop_spec["name"]):
                        self.extract_nested_dependencies(nested_dependencies, prop_spec["type"])
            case _:
                raise ValueError(f'Unsupported Type kind: {spec}')

    def generate_nested_variant(
        self,
        nested_names: Deque[str],
        spec_or_items: Union[Dict[str, Any], List[Dict[str, Any]]]
    ) -> None:
        with self.nested_names_as(nested_names) as nested_name:
            if isinstance(spec_or_items, dict):
                spec = spec_or_items
                item_specs = spec["items"]
                self.symbols[nested_name] = ("union", spec)
            else:
                spec = None
                item_specs = spec_or_items
            self.write(f'enum class {nested_name}Type {{')
            with self.indent():
                for index, item_spec in enumerate(item_specs):
                    with self.nest_name(str(index)):
                        self.inline(indent=True)
                        self.generate_variant_enumeration(item_spec)
                        self.inline(',', end='\n')
            self.write('};')
            self.newline()
            self.write(f'extern std::map<{nested_name}Type, std::string> {nested_name}TypeNames;')
            self.newline()
            if spec is not None:
                spec_docs = spec.get("documentation", None)
                if spec_docs is None and nested_name in self.symbols:
                    _, sym_spec = self.symbols[nested_name]
                    spec_docs = sym_spec.get("documentation", None)
                self.generate_docstring(spec_docs)
            self.write('typedef std::variant<')
            with self.indent():
                k = len(item_specs)
                if k > 0:
                    item_spec = item_specs[0]
                    with self.nest_name(str(0)):
                        self.inline(indent=True)
                        self.generate_type_declaration(item_spec)
                    if k > 1:
                        for i in range(1, k):
                            item_spec = item_specs[i]
                            with self.nest_name(str(i)):
                                self.inline(',', end='\n')
                                self.inline(indent=True)
                                self.generate_type_declaration(item_spec)
            self.newline()
            if nested_name != "LSPAny":
                self.write(f'> {nested_name};')
            else:
                self.write(f'> {nested_name}Base;')
                self.newline()
                self.write('struct LSPAny')
                with self.indent(): self.write(': public LSPAnyBase')
                self.write('{')
                with self.indent():
                    self.write('using LSPAnyBase::variant;')
                self.write('};')
            self.newline()

    def generate_nested_structure(
        self,
        nested_names: Deque[str],
        spec: Dict[str, Any]
    ) -> None:
        with self.nested_names_as(nested_names) as nested_name:
            self.symbols[nested_name] = ("literal", spec)
            self.generate_docstring(spec.get("documentation", None))
            self.write(f'struct {nested_name}')
            self.write('{')
            with self.indent():
                for prop_spec in spec["value"]["properties"]:
                    self.inline(indent=True)
                    self.generate_property(prop_spec)
            self.write('};')
            self.newline()

    def generate_structure(self, struct_spec: Dict[str, Any]) -> None:
        with self.nested_names_as(deque([struct_spec["name"]])) as struct_name:
            nested_dependencies = deque()
            pending_prop_specs = deque()
            pending = deque([struct_spec])
            while len(pending) > 0:
                spec = pending.popleft()
                for prop_spec in reversed(spec["properties"]):
                    with self.nest_name(prop_spec["name"]):
                        self.extract_nested_dependencies(nested_dependencies, prop_spec["type"])
                        pending_prop_specs.append(prop_spec)
                spec_mixins = spec.get("mixins", None)
                if spec_mixins is not None:
                    for type_spec in spec_mixins:
                        mixin_name = type_spec["name"]
                        _, mixin_spec = self.symbols[mixin_name]
                        pending.append(mixin_spec)
            while len(nested_dependencies) > 0:
                nested_names, generate_fn, nested_spec = nested_dependencies.pop()
                generate_fn(nested_names, nested_spec)
                self.add_generated(self.nested_name(nested_names))
            self.generate_docstring(struct_spec.get("documentation", None))
            self.write(f'struct {struct_name}')
            spec_extends = struct_spec.get("extends", None)
            if spec_extends is not None:
                with self.indent():
                    super_iter = iter(spec_extends)
                    super_spec = next(super_iter, None)
                    if super_spec is not None:
                        self.write(f': public {super_spec["name"]}')
                    for super_spec in super_iter:
                        self.write(f', public {super_spec["name"]}')
            self.write('{')
            with self.indent():
                while len(pending_prop_specs) > 0:
                    prop_spec = pending_prop_specs.pop()
                    self.generate_docstring(prop_spec.get("documentation", None))
                    self.inline(indent=True)
                    self.generate_property(prop_spec)
            self.write('};')
            self.newline()

    def generate_structures(self) -> None:
        struct_specs = chain(
            AUXILIARY_SCHEMA["structures"],
            self.schema["structures"],
        )
        for struct_spec in struct_specs:
            struct_name = struct_spec["name"]
            self.symbols[struct_name] = ("structure", struct_spec)
            dependencies = set()
            struct_extends = struct_spec.get("extends", None)
            if struct_extends is not None:
                for type_spec in struct_extends:
                    self.extract_type_dependencies(dependencies, type_spec)
            struct_mixins = struct_spec.get("mixins", None)
            if struct_mixins is not None:
                for type_spec in struct_mixins:
                    self.extract_type_dependencies(dependencies, type_spec)
            for prop_spec in struct_spec["properties"]:
                self.extract_property_dependencies(dependencies, prop_spec)
            self.generate_or_add_to_pending(
                struct_name,
                struct_spec,
                dependencies,
                self.generate_structure
            )

    def generate_type_alias(self, alias_spec: Dict[str, Any]) -> None:
        alias_name = alias_spec["name"]
        with self.nested_names_as(deque([alias_name])):
            type_spec = alias_spec["type"]
            nested_dependencies = deque()
            self.extract_nested_dependencies(nested_dependencies, type_spec)
            while len(nested_dependencies) > 0:
                nested_names, generate_fn, nested_spec = nested_dependencies.pop()
                generate_fn(nested_names, nested_spec)
                self.add_generated(self.nested_name(nested_names))
            if alias_name not in self.generated:
                self.generate_docstring(alias_spec.get("documentation", None))
                match type_spec["kind"]:
                    case "base" | "array" | "map":
                        self.inline('typedef ', indent=True)
                        self.generate_type_declaration(type_spec)
                        self.inline(f' {alias_name};', end='\n')
                        self.newline()
                    case "reference":
                        self.inline('typedef ', indent=True)
                        self.inline(type_spec["name"])
                        self.inline(f' {alias_name};', end='\n')
                        self.newline()
                    case "or":
                        self.generate_nested_variant(self.nested_names, type_spec)
                    case _:
                        raise ValueError(f'Unsupported alias type ({type_spec["kind"]}): {type_spec}')

    def generate_type_aliases(self) -> None:
        alias_specs = chain(
            AUXILIARY_SCHEMA["typeAliases"],
            self.schema["typeAliases"],
        )
        for alias_spec in alias_specs:
            alias_name = alias_spec["name"]
            if alias_name not in self.generated:
                self.symbols[alias_name] = ("alias", alias_spec)
                dependencies = set()
                self.extract_type_dependencies(dependencies, alias_spec["type"])
                self.generate_or_add_to_pending(
                    alias_name,
                    alias_spec,
                    dependencies,
                    self.generate_type_alias
                )

    def generate_request(self, request_spec: Dict[str, Any]) -> None:
        request_method = request_spec["method"]
        request_name = method_to_camel_case(request_method)
        result_name = f'{request_name}Result'
        with self.nested_names_as(deque([result_name])):
            result_spec = request_spec["result"]
            partial_result_spec = request_spec.get("partialResult", None)
            if partial_result_spec is not None:
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
            nested_dependencies = deque()
            if len(result_types) > 1:
                result_type = {
                    "kind": "or",
                    "items": result_types
                }
                self.symbols[result_name] = ("union", result_type)
                self.extract_nested_dependencies(nested_dependencies, result_type)
            else:
                result_type = result_types[0]
                self.symbols[result_name] = ("reference", result_type)
                self.extract_nested_dependencies(nested_dependencies, result_type)
            while len(nested_dependencies) > 0:
                nested_names, generate_fn, nested_spec = nested_dependencies.pop()
                generate_fn(nested_names, nested_spec)
            if len(result_types) == 1:
                if result_name not in self.generated:
                    result_type = result_types[0]
                    self.inline('typedef ', indent=True)
                    if result_type["kind"] == "reference":
                        self.inline(result_type["name"])
                    else:
                        self.generate_type_declaration(result_type)
                    self.inline(f' {result_name};', end='\n')
                    self.newline()

    def generate_requests(self) -> None:
        request_specs = chain(
            AUXILIARY_SCHEMA["requests"],
            self.schema["requests"],
        )
        incoming_requests = []
        outgoing_requests = []
        for request_spec in request_specs:
            request_method = request_spec["method"]
            enumeration = (method_to_underscore(request_method), request_method)
            match request_spec["messageDirection"]:
                case "clientToServer":
                    incoming_requests.append(enumeration)
                case "serverToClient":
                    outgoing_requests.append(enumeration)
            self.symbols[request_method] = ("request", request_spec)
            dependencies = set()
            request_params = request_spec.get("params", None)
            if request_params is not None:
                self.extract_type_dependencies(dependencies, request_params)
            self.extract_type_dependencies(dependencies, request_spec["result"])
            partial_result_type = request_spec.get("partialResult", None)
            if partial_result_type is not None:
                self.extract_type_dependencies(dependencies, partial_result_type)
            self.generate_or_add_to_pending(
                request_method,
                request_spec,
                dependencies,
                self.generate_request
            )
        incoming_requests.sort(key=lambda pair: pair[0])
        outgoing_requests.sort(key=lambda pair: pair[0])
        self.write('enum class IncomingRequest')
        self.write('{')
        with self.indent():
            for enum_name, enum_value in incoming_requests:
                self.write(f'{enum_name},')
        self.write('};')
        self.newline()
        self.write('extern std::map<IncomingRequest, std::string> IncomingRequestNames;')
        self.write('extern std::map<IncomingRequest, std::string> IncomingRequestValues;')
        self.newline()
        self.write(f'auto incomingRequestByName(const std::string &name) -> IncomingRequest;')
        self.write(f'auto incomingRequestByValue(const std::string &value) -> IncomingRequest;')
        self.write(f'auto isIncomingRequest(const std::string &value) -> bool;')
        self.newline()
        self.write('enum class OutgoingRequest')
        self.write('{')
        with self.indent():
            for enum_name, enum_value in outgoing_requests:
                self.write(f'{enum_name},')
        self.write('};')
        self.newline()
        self.write('extern std::map<OutgoingRequest, std::string> OutgoingRequestNames;')
        self.write('extern std::map<OutgoingRequest, std::string> OutgoingRequestValues;')
        self.newline()
        self.write(f'auto outgoingRequestByName(const std::string &name) -> OutgoingRequest;')
        self.write(f'auto outgoingRequestByValue(const std::string &value) -> OutgoingRequest;')
        self.newline()

    def generate_notification(self, notification_spec: Dict[str, Any]) -> None:
        pass

    def generate_notifications(self) -> None:
        notification_specs = chain(
            AUXILIARY_SCHEMA["notifications"],
            self.schema["notifications"],
        )
        incoming_notifications = []
        outgoing_notifications = []
        for notification_spec in notification_specs:
            notification_method = notification_spec["method"]
            enumeration = (method_to_underscore(notification_method), notification_method)
            match notification_spec["messageDirection"]:
                case "clientToServer":
                    incoming_notifications.append(enumeration)
                case "serverToClient":
                    outgoing_notifications.append(enumeration)
            self.symbols[notification_method] = ("notification", notification_spec)
            dependencies = set()
            notification_params = notification_spec.get("params", None)
            if notification_params is not None:
                self.extract_type_dependencies(dependencies, notification_params)
            self.generate_or_add_to_pending(
                notification_method,
                notification_spec,
                dependencies,
                self.generate_notification
            )
        incoming_notifications.sort(key=lambda pair: pair[0])
        outgoing_notifications.sort(key=lambda pair: pair[0])
        self.write('enum class IncomingNotification')
        self.write('{')
        with self.indent():
            for enum_name, enum_value in incoming_notifications:
                self.write(f'{enum_name},')
        self.write('};')
        self.newline()
        self.write('extern std::map<IncomingNotification, std::string> IncomingNotificationNames;')
        self.write('extern std::map<IncomingNotification, std::string> IncomingNotificationValues;')
        self.newline()
        self.write(f'auto incomingNotificationByName(const std::string &name) -> IncomingNotification;')
        self.write(f'auto incomingNotificationByValue(const std::string &value) -> IncomingNotification;')
        self.write(f'auto isIncomingNotification(const std::string &value) -> bool;')
        self.newline()
        self.write('enum class OutgoingNotification')
        self.write('{')
        with self.indent():
            for enum_name, enum_value in outgoing_notifications:
                self.write(f'{enum_name},')
        self.write('};')
        self.newline()
        self.write('extern std::map<OutgoingNotification, std::string> OutgoingNotificationNames;')
        self.write('extern std::map<OutgoingNotification, std::string> OutgoingNotificationValues;')
        self.newline()
        self.write(f'auto outgoingNotificationByName(const std::string &name) -> OutgoingNotification;')
        self.write(f'auto outgoingNotificationByValue(const std::string &value) -> OutgoingNotification;')
        self.newline()

    def generate_lsp_any(self) -> None:
        for alias_spec in self.schema["typeAliases"]:
            alias_name = alias_spec["name"]
            if alias_name == "LSPAny":
                self.symbols[alias_name] = ("alias", alias_spec)
                dependencies = set()
                self.extract_type_dependencies(dependencies, alias_spec["type"])
                self.generate_or_add_to_pending(
                    alias_name,
                    alias_spec,
                    dependencies,
                    self.generate_type_alias
                )
                break

    def generate_lsp_object(self) -> None:
        for alias_spec in self.schema["typeAliases"]:
            alias_name = alias_spec["name"]
            if alias_name == "LSPObject":
                self.symbols[alias_name] = ("alias", alias_spec)
                dependencies = set()
                self.extract_type_dependencies(dependencies, alias_spec["type"])
                dependencies.remove("LSPAny")
                self.generate_or_add_to_pending(
                    alias_name,
                    alias_spec,
                    dependencies,
                    self.generate_type_alias
                )
                break

    def generate_lsp_array(self) -> None:
        for alias_spec in self.schema["typeAliases"]:
            alias_name = alias_spec["name"]
            if alias_name == "LSPArray":
                self.symbols[alias_name] = ("alias", alias_spec)
                dependencies = set()
                self.extract_type_dependencies(dependencies, alias_spec["type"])
                dependencies.remove("LSPAny")
                self.generate_or_add_to_pending(
                    alias_name,
                    alias_spec,
                    dependencies,
                    self.generate_type_alias
                )
                break

    def generate_code(self) -> None:
        print(f'Generating: {self.file_path}')
        version: str = self.schema["metaData"]["version"]

        lower_index = 0
        upper_index = version.index(".", lower_index)
        major_version: str = version[lower_index:upper_index]

        lower_index = upper_index + 1
        upper_index = version.index(".", lower_index)
        minor_version: str = version[lower_index:upper_index]

        lower_index = upper_index + 1
        upper_index = len(version)
        micro_version: str = version[lower_index:upper_index]

        self.generate_disclaimer()
        self.write('#pragma once')
        self.newline()
        self.write('#include <cstddef>')
        self.write('#include <map>')
        self.write('#include <memory>')
        self.write('#include <optional>')
        self.write('#include <string>')
        self.write('#include <tuple>')
        self.write('#include <utility>')
        self.write('#include <variant>')
        self.write('#include <vector>')
        self.newline()
        self.write('/**')
        self.write(f' * Interface definitions from the LSP {version} specification.')
        self.write(f' * See: https://microsoft.github.io/language-server-protocol/specifications/lsp/{major_version}.{minor_version}/specification')
        self.write(' */')
        self.write(f'namespace {self.namespace} {{')
        with self.indent():
            self.write('const std::string JSON_RPC_VERSION = "2.0";')
            self.write(f'const std::string LSP_VERSION = "{version}";')
            self.newline()
            self.write(f'typedef int {rename_type("integer")};')
            self.write(f'typedef unsigned int {rename_type("uinteger")};')
            self.write(f'typedef double {rename_type("decimal")};')
            self.write(f'typedef bool {rename_type("boolean")};')
            self.write(f'typedef std::nullptr_t {rename_type("null")};')
            self.write(f'typedef std::string {rename_type("string")};')
            self.newline()
            self.write(f'typedef {rename_type("string")} URI;')
            self.write(f'typedef {rename_type("string")} DocumentUri;')
            self.write(f'typedef {rename_type("string")} RegExp;')

            self.generated.update([
                "bool",
                "boolean",
                "decimal",
                "DocumentUri",
                "double",
                "int",
                "integer",
                "null",
                "RegExp",
                "std::nullptr_t",
                "std::string",
                "string",
                "uinteger",
                "unsigned int",
                "URI",
            ])

            self.newline()
            self.write('struct LSPAny;    // Forward declaration')
            self.newline()
            self.generate_lsp_object()
            self.generate_lsp_array()
            self.generate_lsp_any()
            self.generate_enumerations()
            self.generate_structures()
            self.generate_type_aliases()
            self.generate_requests()
            self.generate_notifications()
            self.resolve_cycles()
            if len(self.dependencies) > 0:
                raise RuntimeError(f'There are {len(self.dependencies)} symbols remaining to be generated.')

        self.write(f'}} // namespace {self.namespace}')
