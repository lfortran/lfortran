#ifndef MESSAGE_HANDLER_HPP
#define MESSAGE_HANDLER_HPP

#include <string>

#include <libasr/asr.h>

namespace LCompilers {

       struct error_highlight {
              std::string message;
              uint32_t first_line;
              uint32_t first_column;
              uint32_t last_line;
              uint32_t last_column;
              std::string filename;
              uint32_t severity;
       };
       struct document_symbols {
              std::string symbol_name;
              uint32_t first_line;
              uint32_t first_column;
              uint32_t last_line;
              uint32_t last_column;
              std::string filename;
              ASR::symbolType symbol_type;
       };

} // namespace LCompilers

#endif
