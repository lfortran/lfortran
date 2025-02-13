from typing import (
    Any,
    Dict,
)

AUXILIARY_SCHEMA: Dict[str, Any] = {
    "enumerations": [
    ],
    "structures": [
        {
            "name": "Message",
            "properties": [
                {
                    "name": "jsonrpc",
                    "type": {
                        "kind": "base",
                        "name": "string",
                    },
                },
            ],
            "documentation": 'A general message as defined by JSON-RPC. The language server protocol\nalways uses “2.0” as the jsonrpc version.',
        },
        {
            "name": "RequestMessage",
            "properties": [
                {
                    "name": "id",
                    "type": {
                        "kind": "reference",
                        "name": "RequestId",
                    },
                    "documentation": "The request id.",
                },
                {
                    "name": "method",
                    "type": {
                        "kind": "base",
                        "name": "string",
                    },
                    "documentation": "The method to be invoked.",
                },
                {
                    "name": "params",
                    "type": {
                        "kind": "reference",
                        "name": "MessageParams",
                    },
                    "documentation": "The method's params.",
                    "optional": True,
                },
            ],
            "extends": [
                {
                    "kind": "reference",
                    "name": "Message",
                }
            ],
            "documentation": "A request message to describe a request between the client and the server.\nEvery processed request must send a response back to the sender of the\nrequest.",
        },
        {
            "name": "NotificationMessage",
            "properties": [
                {
                    "name": "method",
                    "type": {
                        "kind": "base",
                        "name": "string",
                    },
                    "documentation": "The method to be invoked.",
                },
                {
                    "name": "params",
                    "type": {
                        "kind": "reference",
                        "name": "MessageParams",
                    },
                    "documentation": "The notification's params.",
                    "optional": True,
                }
            ],
            "extends": [
                {
                    "kind": "reference",
                    "name": "Message",
                }
            ]
        },
        {
            "name": "ResponseError",
            "properties": [
                {
                    "name": "code",
                    "type": {
                        "kind": "base",
                        "name": "integer",
                    },
                    "documentation": "A number indicating the error type that occurred.",
                },
                {
                    "name": "message",
                    "type": {
                        "kind": "base",
                        "name": "string",
                    },
                    "documentation": "A string providing a short description of the error.",
                },
                {
                    "name": "data",
                    "type": {
                        "kind": "reference",
                        "name": "LSPAny",
                    },
                    "documentation": "A primitive or structured value that contains additional information about\nthe error. Can be omitted.",
                    "optional": True,
                },
            ],
        },
        {
            "name": "ResponseMessage",
            "properties": [
                {
                    "name": "id",
                    "type": {
                        "kind": "reference",
                        "name": "ResponseId",
                    },
                    "documentation": "The request id.",
                },
                {
                    "name": "result",
                    "type": {
                        "kind": "reference",
                        "name": "LSPAny",
                    },
                    "documentation": "The result of a request. This member is REQUIRED on success. This member\nMUST NOT exist if there was an error invoking the method.",
                    "optional": True,
                },
                {
                    "name": "error",
                    "type": {
                        "kind": "reference",
                        "name": "ResponseError",
                    },
                    "documentation": "The error object in case a request fails.",
                    "optional": True,
                },
            ],
            "extends": [
                {
                    "kind": "reference",
                    "name": "Message",
                },
            ],
            "documentation": "A Response Message sent as a result of a request. If a request doesn’t\nprovide a result value the receiver of a request still needs to return a\nresponse message to conform to the JSON-RPC specification. The result\nproperty of the ResponseMessage should be set to null in this case to signal\na successful request.",
        },
    ],
    "typeAliases": [
        {
            "name": "integer",
            "type": {
                "kind": "base",
                "name": "int",
            },
        },
        {
            "name": "uinteger",
            "type": {
                "kind": "base",
                "name": "unsigned int",
            },
        },
        {
            "name": "decimal",
            "type": {
                "kind": "base",
                "name": "double",
            },
        },
        {
            "name": "boolean",
            "type": {
                "kind": "base",
                "name": "bool",
            },
        },
        {
            "name": "null",
            "type": {
                "kind": "base",
                "name": "std::nullptr_t",
            },
        },
        {
            "name": "string",
            "type": {
                "kind": "base",
                "name": "std::string",
            },
        },
        {
            "name": "URI",
            "type": {
                "kind": "base",
                "name": "string",
            },
        },
        {
            "name": "DocumentUri",
            "type": {
                "kind": "base",
                "name": "string",
            },
        },
        {
            "name": "RegExp",
            "type": {
                "kind": "base",
                "name": "string",
            },
        },
        {
            "name": "RequestId",
            "type": {
                "kind": "or",
                "items": [
                    {
                        "kind": "base",
                        "name": "integer",
                    },
                    {
                        "kind": "base",
                        "name": "string",
                    },
                ],
            },
        },
        {
            "name": "MessageParams",
            "type": {
                "kind": "or",
                "items": [
                    {
                        "kind": "reference",
                        "name": "LSPArray",
                    },
                    {
                        "kind": "reference",
                        "name": "LSPObject",
                    },
                ],
            },
            "documentation": "A request message to describe a request between the client and the server.\nEvery processed request must send a response back to the sender of the\nrequest.",
        },
        {
            "name": "ResponseId",
            "type": {
                "kind": "or",
                "items": [
                    {
                        "kind": "base",
                        "name": "integer",
                    },
                    {
                        "kind": "base",
                        "name": "string",
                    },
                    {
                        "kind": "base",
                        "name": "null",
                    },
                ],
            },
        },
    ],
    "requests": [
    ],
    "notifications": [
    ],
}
