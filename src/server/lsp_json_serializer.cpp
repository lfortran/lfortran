#include <utility>

#include <server/specification.h>
#include <server/lsp_exception.h>
#include <server/lsp_json_serializer.h>

namespace LCompilers::LanguageServerProtocol {

  std::string LspJsonSerializer::serialize(const LSPAny &any) const {
    std::string buffer;
    buffer.reserve(4096);
    LSPAnyType anyType = static_cast<LSPAnyType>(any.index());
    switch (anyType) {
    case LSPAnyType::OBJECT_TYPE: {
      serializeObject(buffer, std::get<LSPObject>(any));
      break;
    }
    case LSPAnyType::ARRAY_TYPE: {
      serializeArray(buffer, std::get<LSPArray>(any));
      break;
    }
    default: {
      throw LSP_EXCEPTION(
        ErrorCodes::PARSE_ERROR,
        ("JSON root must be of type LSPAnyType::OBJECT_TYPE or "
         "LSPAnyType::ARRAY_TYPE, not LSPAnyType::" +
         LSPAnyTypeNames.at(anyType))
      );
    }
    }
    return buffer;
  }

  void LspJsonSerializer::serializeArray(
    std::string &buffer,
    const LSPArray &array
  ) const {
    buffer.push_back('[');
    LSPArray::const_iterator iter = array.begin();
    if (iter != array.end()) {
      serializeValue(buffer, **iter++);
      while (iter != array.end()) {
        buffer.push_back(',');
        serializeValue(buffer, **iter++);
      }
    }
    buffer.push_back(']');
  }

  void LspJsonSerializer::serializeObject(
    std::string &buffer,
    const LSPObject &object
  ) const {
    buffer.push_back('{');
    LSPObject::const_iterator iter = object.begin();
    if (iter != object.end()) {
      serializeString(buffer, iter->first);
      buffer.push_back(':');
      serializeValue(buffer, *iter->second);
      while ((++iter) != object.end()) {
        buffer.push_back(',');
        serializeString(buffer, iter->first);
        buffer.push_back(':');
        serializeValue(buffer, *iter->second);
      }
    }
    buffer.push_back('}');
  }

  void LspJsonSerializer::serializeValue(
    std::string &buffer,
    const LSPAny &value
  ) const {
    switch (static_cast<LSPAnyType>(value.index())) {
    case LSPAnyType::OBJECT_TYPE: {
      serializeObject(buffer, std::get<LSPObject>(value));
      break;
    }
    case LSPAnyType::ARRAY_TYPE: {
      serializeArray(buffer, std::get<LSPArray>(value));
      break;
    }
    case LSPAnyType::STRING_TYPE: {
      serializeString(buffer, value);
      break;
    }
    case LSPAnyType::INTEGER_TYPE: // fallthrough
    case LSPAnyType::UINTEGER_TYPE: // fallthrough
    case LSPAnyType::DECIMAL_TYPE: {
      serializeNumber(buffer, value);
      break;
    }
    case LSPAnyType::BOOLEAN_TYPE: {
      serializeBoolean(buffer, value);
      break;
    }
    case LSPAnyType::NULL_TYPE: {
      serializeNull(buffer, value);
      break;
    }
    }
  }

  void LspJsonSerializer::serializeString(
    std::string &buffer,
    const LSPAny &value
  ) const {
    LSPAnyType valueType = static_cast<LSPAnyType>(value.index());
    switch (valueType) {
    case LSPAnyType::STRING_TYPE: {
      const string_t &string = std::get<string_t>(value);
      buffer.push_back('"');
      for (std::size_t i = 0, k = string.length(); i < k; ++i) {
        unsigned char c = string[i];
        switch (c) {
        case '"': {
          buffer.append("\\\"");
          break;
        }
        case '\\': {
          buffer.append("\\\\");
          break;
        }
        case '\n': {
          buffer.append("\\n");
          break;
        }
        case '\t': {
          buffer.append("\\t");
          break;
        }
        case '\b': {
          buffer.append("\\b");
          break;
        }
        case '\r': {
          buffer.append("\\r");
          break;
        }
        case '\f': {
          buffer.append("\\f");
          break;
        }
        default: {
          buffer.push_back(c);
        }
        }
      }
      buffer.push_back('"');
      break;
    }
    default: {
      throw LSP_EXCEPTION(
        ErrorCodes::INVALID_PARAMS,
        ("Cannot serialize JSON string of type " +
         LSPAnyTypeNames.at(valueType))
      );
    }
    }
  }

  void LspJsonSerializer::serializeNumber(
    std::string &buffer,
    const LSPAny &value
  ) const {
    LSPAnyType valueType = static_cast<LSPAnyType>(value.index());
    switch (valueType) {
    case LSPAnyType::INTEGER_TYPE: {
      buffer.append(std::to_string(std::get<integer_t>(value)));
      break;
    }
    case LSPAnyType::UINTEGER_TYPE: {
      buffer.append(std::to_string(std::get<uinteger_t>(value)));
      break;
    }
    case LSPAnyType::DECIMAL_TYPE: {
      buffer.append(std::to_string(std::get<decimal_t>(value)));
      break;
    }
    default: {
      throw LSP_EXCEPTION(
        ErrorCodes::INVALID_PARAMS,
        ("Cannot serialize JSON number of type " +
         LSPAnyTypeNames.at(valueType))
      );
    }
    }
  }

  void LspJsonSerializer::serializeBoolean(
    std::string &buffer,
    const LSPAny &value
  ) const {
    LSPAnyType valueType = static_cast<LSPAnyType>(value.index());
    switch (valueType) {
    case LSPAnyType::BOOLEAN_TYPE: {
      buffer.append(std::to_string(std::get<boolean_t>(value)));
      break;
    }
    default: {
      throw LSP_EXCEPTION(
        ErrorCodes::INVALID_PARAMS,
        ("Cannot serialize JSON boolean of type " +
         LSPAnyTypeNames.at(valueType))
      );
    }
    }
  }

  void LspJsonSerializer::serializeNull(
    std::string &buffer,
    const LSPAny &value
  ) const {
    LSPAnyType valueType = static_cast<LSPAnyType>(value.index());
    switch (valueType) {
    case LSPAnyType::NULL_TYPE: {
      buffer.append("null");
      break;
    }
    default: {
      throw LSP_EXCEPTION(
        ErrorCodes::INVALID_PARAMS,
        ("Cannot serialize JSON null of type " +
         LSPAnyTypeNames.at(valueType))
      );
    }
    }
  }

} // namespace LCompilers::LanguageServerProtocol
