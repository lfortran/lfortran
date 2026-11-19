#ifndef LFORTRAN_PARSER_PARSER_EXCEPTION_H
#define LFORTRAN_PARSER_PARSER_EXCEPTION_H

#include <libasr/exception.h>

namespace LCompilers::LFortran {

namespace parser_local {

    // Local exceptions that are used to terminate the parser.
    // It is not propagated outside.
    // This file is included in the parser and the tokenizers. Nowhere else.
    class TokenizerAbort { 
    };

    class ParserAbort {
    };

}

} // namespace LCompilers::LFortran


#endif
