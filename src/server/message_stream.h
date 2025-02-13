#pragma once

namespace LCompilers::LLanguageServer {

    class MessageStream {
    public:
        virtual std::string next() = 0;
    };

} // namespace LCompilers::LLanguageServer
