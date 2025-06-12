#ifndef LFORTRAN_PARSER_PARSER_EXCEPTION_H
#define LFORTRAN_PARSER_PARSER_EXCEPTION_H

#include <libasr/exception.h>

namespace LCompilers::LFortran {

namespace parser_local {

    // Local exceptions that are used to terminate the parser.
    // It is not propagated outside.
    // This file is included in parser.tab.cc (via semantics.h)
    // And in parser.cpp. Nowhere else.
    class TokenizerAbort { 
    };

    class ParserAbort {
    };

}

} // namespace LCompilers::LFortran


#endif
