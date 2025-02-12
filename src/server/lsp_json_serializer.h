#pragma once

#include <server/specification.h>

namespace LCompilers::LanguageServerProtocol {

  class LspJsonSerializer {
  public:
    std::string serialize(const LSPAny &any) const;
  private:
    void serializeArray(std::string &buffer, const LSPArray &array) const;
    void serializeObject(std::string &buffer, const LSPObject &object) const;
    void serializeValue(std::string &buffer, const LSPAny &value) const;
    void serializeString(std::string &buffer, const LSPAny &value) const;
    void serializeNumber(std::string &buffer, const LSPAny &value) const;
    void serializeBoolean(std::string &buffer, const LSPAny &value) const;
    void serializeNull(std::string &buffer, const LSPAny &value) const;
  };

} // namespace LCompilers::LanguageServerProtocol
