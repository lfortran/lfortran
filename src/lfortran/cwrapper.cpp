#include <cstdlib>
#include <cstring>

#include <lfortran/cwrapper.h>
#include <lfortran/ast.h>
#include <libasr/alloc.h>
#include <lfortran/parser/parser.h>
#include <lfortran/pickle.h>


extern "C" {

#define CWRAPPER_BEGIN try {

#define CWRAPPER_END                                                           \
    return LFORTRAN_NO_EXCEPTION;                                              \
    }                                                                          \
    catch (LCompilers::LCompilersException & e)                                    \
    {                                                                          \
        return e.error_code();                                                 \
    }                                                                          \
    catch (...)                                                                \
    {                                                                          \
        return LFORTRAN_RUNTIME_ERROR;                                         \
    }




struct LFortranCParser {
    Allocator al;
    LFortranCParser() : al{1024*1024} {}
};
struct lfortran_ast_t {
    LCompilers::LFortran::AST::ast_t m;
};


LFortranCParser *lfortran_parser_new()
{
    return new LFortranCParser;
}

void lfortran_parser_free(LFortranCParser *self)
{
    delete self;
}

lfortran_exceptions_t lfortran_parser_parse(LFortranCParser *self,
        const char *input, lfortran_ast_t **ast)
{
    CWRAPPER_BEGIN

    LCompilers::LFortran::AST::ast_t* result;
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::Result<LCompilers::LFortran::AST::TranslationUnit_t*> res
        = LCompilers::LFortran::parse(self->al, input, diagnostics);
    if (res.ok) {
        result = res.result->m_items[0];
    } else {
        return LFORTRAN_PARSER_ERROR;
    }
    lfortran_ast_t* result2 = (lfortran_ast_t*)result;
    *ast = result2;

    CWRAPPER_END
}

lfortran_exceptions_t lfortran_parser_pickle(lfortran_ast_t* ast,
        char **str)
{
    CWRAPPER_BEGIN

    std::string p = LCompilers::LFortran::pickle(ast->m);
    *str = new char[p.length()+1];
    std::strcpy(*str, p.c_str());

    CWRAPPER_END
}

lfortran_exceptions_t lfortran_str_free(char *str)
{
    CWRAPPER_BEGIN

    delete[] str;

    CWRAPPER_END
}


} // extern "C"
