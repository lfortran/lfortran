#include <tests/doctest.h>

#include <string>

#include <lfortran/fortran_evaluator.h>
#include <lfortran/parser/source_offset.h>
#include <lfortran/utils.h>
#include <libasr/asr.h>

using LCompilers::FortranEvaluator;
using LCompilers::CompilerOptions;

namespace {

// Fortran has no loop-else, so move the statement that follows the first
// loop of the function `name` into the loop-else of that loop.
void seed_loop_else(Allocator &al, LCompilers::ASR::TranslationUnit_t &asr,
        const std::string &name) {
    namespace ASR = LCompilers::ASR;
    ASR::symbol_t *sym = asr.m_symtab->resolve_symbol(name);
    REQUIRE(sym != nullptr);
    ASR::Function_t *fn = ASR::down_cast<ASR::Function_t>(sym);
    size_t loop = fn->n_body;
    for (size_t i = 0; i < fn->n_body; i++) {
        if (ASR::is_a<ASR::DoLoop_t>(*fn->m_body[i])
                || ASR::is_a<ASR::WhileLoop_t>(*fn->m_body[i])) {
            loop = i;
            break;
        }
    }
    REQUIRE(loop + 1 < fn->n_body);
    ASR::stmt_t **orelse = al.allocate<ASR::stmt_t*>(1);
    orelse[0] = fn->m_body[loop + 1];
    if (ASR::is_a<ASR::DoLoop_t>(*fn->m_body[loop])) {
        ASR::DoLoop_t *x = ASR::down_cast<ASR::DoLoop_t>(fn->m_body[loop]);
        x->m_orelse = orelse;
        x->n_orelse = 1;
    } else {
        ASR::WhileLoop_t *x = ASR::down_cast<ASR::WhileLoop_t>(
            fn->m_body[loop]);
        x->m_orelse = orelse;
        x->n_orelse = 1;
    }
    for (size_t i = loop + 1; i + 1 < fn->n_body; i++) {
        fn->m_body[i] = fn->m_body[i + 1];
    }
    fn->n_body--;
}

} // namespace

TEST_CASE("C and C++ write a loop-else that an EXIT skips") {
    std::string source = R"(
integer function f_do()
    integer :: i
    f_do = 0
    do i = 1, 3
        if (i == 2) exit
        f_do = f_do + 1
    end do
    f_do = f_do + 10
end function

integer function f_while()
    integer :: s
    s = 0
    do while (s < 3)
        s = s + 1
        if (s == 5) exit
    end do
    s = s + 40
    f_while = s
end function
)";
    for (bool c : {true, false}) {
        Allocator al(64*1024);
        LCompilers::diag::Diagnostics diagnostics;
        CompilerOptions co;
        co.po.runtime_library_dir =
            LCompilers::LFortran::get_runtime_library_dir();
        co.platform = LCompilers::get_platform();
        FortranEvaluator evaluator(co);
        LCompilers::LocationManager lm;
        LCompilers::LocationManager::FileLocations file;
        file.in_filename = "loop_else.f90";
        lm.files.push_back(file);
        lm.file_ends.push_back(
            LCompilers::LFortran::source_offset(source.size()));
        auto asr = evaluator.get_asr2(source, lm, diagnostics);
        INFO(diagnostics.render2());
        REQUIRE(asr.ok);
        seed_loop_else(al, *asr.result, "f_do");
        seed_loop_else(al, *asr.result, "f_while");

        LCompilers::Result<std::string> out = c
            ? evaluator.get_c2(*asr.result, diagnostics, 1)
            : evaluator.get_cpp2(*asr.result, diagnostics, 1);
        INFO(diagnostics.render2());
        REQUIRE(out.ok);
        INFO(out.result);
        // An EXIT of the loop goes past its loop-else, which follows the
        // loop and runs when it completes.
        CHECK(out.result.find(R"(
    for (i = 1; i <= 3; i++) {
        if (i == 2) {
            goto __lfortran_exit_0;
        }
        f_do = f_do + 1;
    }
    f_do = f_do + 10;
    __lfortran_exit_0: ;
)") != std::string::npos);
        CHECK(out.result.find(R"(
    while (s < 3) {
        s = s + 1;
        if (s == 5) {
            goto __lfortran_exit_1;
        }
    }
    s = s + 40;
    __lfortran_exit_1: ;
)") != std::string::npos);
    }
}
