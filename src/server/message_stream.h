#pragma once

#include <string>

namespace LCompilers::LLanguageServer {

    class MessageStream {
    public:
        MessageStream();
        virtual ~MessageStream();
        virtual std::string next(bool &exit) = 0;
    };

} // namespace LCompilers::LLanguageServer
