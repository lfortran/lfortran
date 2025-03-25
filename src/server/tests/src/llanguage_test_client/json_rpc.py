from dataclasses import dataclass
from typing import ClassVar, Dict, List, Optional, Union

JsonObject = Dict[str, "JsonValue"]
JsonArray = List["JsonValue"]
JsonValue = Union[JsonObject, JsonArray, str, int, float, bool, None]

RequestId = Union[str, int]
ResponseId = Optional[RequestId]

MessageParams = Union[JsonArray, JsonObject]


JSON_RPC_VERSION: str = "2.0"


@dataclass
class Message:
    jsonrpc: ClassVar[str] = JSON_RPC_VERSION


@dataclass
class RequestMessage(Message):
    id: RequestId
    method: str
    params: Optional[MessageParams]


@dataclass
class NotificationMessage(Message):
    method: str
    params: Optional[MessageParams]


@dataclass
class ResponseError:
    code: int
    message: str
    data: Optional[JsonValue]


@dataclass
class ResponseMessage(Message):
    id: ResponseId
    result: Optional[JsonValue]
    error: Optional[ResponseError]
