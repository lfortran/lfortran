from collections import deque
from itertools import chain
from pathlib import Path
from typing import Any, Callable, Deque, Dict, List, Set, Tuple, Union

from llanguage_server.auxiliary_schema import AUXILIARY_SCHEMA
from llanguage_server.cxx.file_generator import CPlusPlusLspFileGenerator
from llanguage_server.utils import (as_enumeration_spec, lower_first,
                                    method_to_underscore, rename_enum,
                                    rename_type)


class CPlusPlusSpecificationSourceGenerator(CPlusPlusLspFileGenerator):
    generated: Set[str]

    def __init__(
        self,
        output_dir: Path,
        schema: Dict[str, Any],
        namespace: str,
        symbols: Dict[str, Tuple[str, Dict[str, Any]]]
    ) -> None:
        specification_source = output_dir / "specification.cpp"
        super().__init__(specification_source, schema, namespace, symbols)
        self.generated = set()

    def generate_enumeration(self, enum_spec: Dict[str, Any]) -> None:
        enum_name = enum_spec["name"]
        lower_name = lower_first(enum_name)
        value_type = rename_type(enum_spec["type"]["name"])
        self.write('std::map<')
        with self.indent(): self.write(f'{enum_name},')
        with self.indent(): self.write('std::string')
        self.write(f'> {enum_name}Names = {{')
        with self.indent():
                for value in enum_spec["values"]:
                    value_name = rename_enum(value["name"])
                    self.write(f'{{{enum_name}::{value_name}, "{value_name}"}},')
        self.write('};')
        self.newline()
        if value_type == rename_type("string"):
            self.write('std::map<')
            with self.indent(): self.write(f'{enum_name},')
            with self.indent(): self.write(value_type)
            self.write(f'> {enum_name}Values = {{')
            with self.indent():
                match value_type:
                    case "string_t":
                        for value in enum_spec["values"]:
                            value_name = rename_enum(value["name"])
                            self.write(f'{{{enum_name}::{value_name}, "{value["value"]}"}},')
                    case _:
                        for value in enum_spec["values"]:
                            value_name = rename_enum(value["name"])
                            self.write(f'{{{enum_name}::{value_name}, {value["value"]}}},')
            self.write('};')
            self.newline()
        self.write(f'auto {lower_name}ByName(')
        with self.indent(): self.write('const std::string &name')
        self.write(f') -> {enum_name} {{')
        with self.indent():
            self.write(f'for (const auto &[enum_name, enum_value]')
            with self.indent(2): self.write(f': {enum_name}Names) {{')
            with self.indent():
                self.write('if (name == enum_value) {')
                with self.indent(): self.write('return enum_name;')
                self.write('}')
            self.write('}')
            self.write('throw std::invalid_argument(')
            with self.indent():
                self.write(f'"Invalid {enum_name} name: " + name')
            self.write(');')
        self.write('}')
        self.newline()
        if value_type == rename_type("string"):
            self.write(f'auto {lower_name}ByValue(')
            with self.indent(): self.write(f'const {value_type} &value')
            self.write(f') -> {enum_name} {{')
            with self.indent():
                self.write(f'for (const auto &[enum_name, enum_value]')
                with self.indent(2): self.write(f': {enum_name}Values) {{')
                with self.indent():
                    self.write('if (value == enum_value) {')
                    with self.indent(): self.write('return enum_name;')
                    self.write('}')
                self.write('}')
                self.write('throw std::invalid_argument(')
                with self.indent():
                    self.write(f'"Invalid {enum_name} value: " + value')
                self.write(');')
            self.write('}')
        else:
            self.write(f'auto {lower_name}ByValue(')
            with self.indent(): self.write(f'{value_type} value')
            self.write(f') -> {enum_name} {{')
            with self.indent():
                self.write(f'for (const auto &[field_name, field_value]')
                with self.indent(2): self.write(f': {enum_name}Names) {{')
                with self.indent():
                    self.write(f'if (value == static_cast<{value_type}>(field_name)) {{')
                    with self.indent(): self.write('return field_name;')
                    self.write('}')
                self.write('}')
                self.write('throw std::invalid_argument(')
                with self.indent():
                    self.write(f'"Invalid {enum_name} value: " + std::to_string(value)')
                self.write(');')
            self.write('}')
        self.newline()

    def generate_enumerations(self) -> None:
        enum_specs = chain(
            AUXILIARY_SCHEMA["enumerations"],
            self.schema["enumerations"],
        )
        for enum_spec in enum_specs:
            self.generate_enumeration(enum_spec)

    def generate_structure(self, struct_spec: Dict[str, Any]) -> None:
        with self.nested_names_as(deque([struct_spec["name"]])) as struct_name:
            nested_dependencies = deque()
            pending = deque([struct_spec])
            while len(pending) > 0:
                spec = pending.popleft()
                for prop_spec in reversed(spec["properties"]):
                    with self.nest_name(prop_spec["name"]):
                        self.extract_nested_dependencies(nested_dependencies, prop_spec["type"])
                spec_mixins = spec.get("mixins", None)
                if spec_mixins is not None:
                    for type_spec in spec_mixins:
                        mixin_name = type_spec["name"]
                        _, mixin_spec = self.symbols[mixin_name]
                        pending.append(mixin_spec)
            while len(nested_dependencies) > 0:
                nested_names, generate_fn, nested_spec = nested_dependencies.pop()
                generate_fn(nested_names, nested_spec)
                self.generated.add(self.nested_name(nested_names))

    def generate_structures(self) -> None:
        struct_specs = chain(
            AUXILIARY_SCHEMA["structures"],
            self.schema["structures"],
        )
        for struct_spec in struct_specs:
            self.generate_structure(struct_spec)

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
            self.write(f'std::map<{nested_name}Type, std::string> {nested_name}TypeNames = {{')
            with self.indent():
                for index, item_spec in enumerate(item_specs):
                    with self.nest_name(str(index)):
                        self.inline(f'{{{nested_name}Type::', indent=True)
                        self.generate_variant_enumeration(item_spec)
                        self.inline(', "')
                        self.generate_variant_enumeration(item_spec)
                        self.inline('"},', end='\n')
            self.write('};')
            self.newline()

    def generate_nested_structure(
        self,
        nested_names: Deque[str],
        spec: Dict[str, Any]
    ) -> None:
        with self.nested_names_as(nested_names) as nested_name:
            pass

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

    def generate_type_alias(self, alias_spec: Dict[str, Any]) -> None:
        alias_name = alias_spec["name"]
        with self.nested_names_as(deque([alias_name])):
            type_spec = alias_spec["type"]
            nested_dependencies = deque()
            self.extract_nested_dependencies(nested_dependencies, type_spec)
            while len(nested_dependencies) > 0:
                nested_names, generate_fn, nested_spec = nested_dependencies.pop()
                generate_fn(nested_names, nested_spec)
                self.generated.add(self.nested_name(nested_names))
            if alias_name not in self.generated:
                match type_spec["kind"]:
                    case "base" | "array" | "map" | "reference":
                        pass
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
            self.generate_type_alias(alias_spec)

    def generate_request(self, request_spec: Dict[str, Any]) -> None:
        pass

    def generate_is_incoming_request(self) -> None:
        self.write('auto isIncomingRequest(const std::string &value) -> bool {')
        with self.indent():
            self.write('for (const auto &[enum_key, enum_value] : IncomingRequestValues) {')
            with self.indent():
                self.write('if (value == enum_value) {')
                with self.indent():
                    self.write('return true;')
                self.write('}')
            self.write('}')
            self.write('return false;')
        self.write('}')
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
            self.generate_request(request_spec)
        self.generate_enumeration(
            as_enumeration_spec("IncomingRequest", incoming_requests)
        )
        self.generate_is_incoming_request()
        self.generate_enumeration(
            as_enumeration_spec("OutgoingRequest", outgoing_requests)
        )

    def generate_notification(self, notification_spec: Dict[str, Any]) -> None:
        pass

    def generate_is_incoming_notification(self) -> None:
        self.write('auto isIncomingNotification(const std::string &value) -> bool {')
        with self.indent():
            self.write('for (const auto &[enum_key, enum_value] : IncomingNotificationValues) {')
            with self.indent():
                self.write('if (value == enum_value) {')
                with self.indent():
                    self.write('return true;')
                self.write('}')
            self.write('}')
            self.write('return false;')
        self.write('}')
        self.newline()

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
            self.generate_notification(notification_spec)
        self.generate_enumeration(
            as_enumeration_spec("IncomingNotification", incoming_notifications)
        )
        self.generate_is_incoming_notification()
        self.generate_enumeration(
            as_enumeration_spec("OutgoingNotification", outgoing_notifications)
        )

    def generate_code(self) -> None:
        print(f'Generating: {self.file_path}')
        self.generate_disclaimer()
        self.write('#include <stdexcept>')
        self.newline()
        self.write('#include <server/specification.h>')
        self.newline()
        self.write(f'namespace {self.namespace} {{')
        with self.indent():
            self.generate_enumerations()
            self.generate_structures()
            self.generate_type_aliases()
            self.generate_requests()
            self.generate_notifications()
        self.write(f'}} // namespace {self.namespace}')
