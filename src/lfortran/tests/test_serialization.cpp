#include <tests/doctest.h>
#include <iostream>

#include <libasr/bwriter.h>
#include <libasr/serialization.h>
#include <lfortran/ast_serialization.h>
#include <libasr/modfile.h>
#include <lfortran/pickle.h>
#include <lfortran/parser/parser.h>
#include <lfortran/semantics/ast_to_asr.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_verify.h>
#include <libasr/utils.h>

using LCompilers::TRY;
using LCompilers::string_to_uint64;
using LCompilers::uint64_to_string;
using LCompilers::string_to_uint32;
using LCompilers::uint32_to_string;

TEST_CASE("Integer conversion") {
    uint64_t i;
    i = 1;
    CHECK(string_to_uint32(uint32_to_string(i)) == i);
    CHECK(string_to_uint64(uint64_to_string(i)) == i);

    i = 150;
    CHECK(string_to_uint32(uint32_to_string(i)) == i);
    CHECK(string_to_uint64(uint64_to_string(i)) == i);

    i = 256;
    CHECK(string_to_uint32(uint32_to_string(i)) == i);
    CHECK(string_to_uint64(uint64_to_string(i)) == i);

    i = 65537;
    CHECK(string_to_uint32(uint32_to_string(i)) == i);
    CHECK(string_to_uint64(uint64_to_string(i)) == i);

    i = 16777217;
    CHECK(string_to_uint32(uint32_to_string(i)) == i);
    CHECK(string_to_uint64(uint64_to_string(i)) == i);

    i = 4294967295LU;
    CHECK(string_to_uint32(uint32_to_string(i)) == i);
    CHECK(string_to_uint64(uint64_to_string(i)) == i);

    i = 4294967296LU;
    CHECK(string_to_uint32(uint32_to_string(i)) != i);
    CHECK(string_to_uint64(uint64_to_string(i)) == i);

    i = 18446744073709551615LLU;
    CHECK(string_to_uint32(uint32_to_string(i)) != i);
    CHECK(string_to_uint64(uint64_to_string(i)) == i);
}

void ast_ser(const std::string &src) {
    Allocator al(4*1024);

    LCompilers::LFortran::AST::TranslationUnit_t* result;
    LCompilers::diag::Diagnostics diagnostics;
    result = TRY(LCompilers::LFortran::parse(al, src, diagnostics));
    std::string ast_orig = LCompilers::LFortran::pickle(*result);
    std::string binary = LCompilers::LFortran::serialize(*result);

    LCompilers::LFortran::AST::ast_t *ast;
    ast = LCompilers::LFortran::deserialize_ast(al, binary);
    CHECK(LCompilers::LFortran::AST::is_a<LCompilers::LFortran::AST::unit_t>(*ast));

    std::string ast_new = LCompilers::LFortran::pickle(*ast);

    CHECK(ast_orig == ast_new);
}

void asr_ser(const std::string &src) {
    Allocator al(4*1024);

    LCompilers::LFortran::AST::TranslationUnit_t* ast0;
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::CompilerOptions compiler_options;
    ast0 = TRY(LCompilers::LFortran::parse(al, src, diagnostics));
    LCompilers::ASR::TranslationUnit_t* asr = TRY(LCompilers::LFortran::ast_to_asr(al, *ast0,
        diagnostics, nullptr, false, compiler_options));

    std::string asr_orig = LCompilers::LFortran::pickle(*asr);
    std::string binary = LCompilers::serialize(*asr);

    LCompilers::ASR::asr_t *asr_new0;
    LCompilers::SymbolTable symtab(nullptr);
    asr_new0 = LCompilers::deserialize_asr(al, binary, true, symtab);
    CHECK(LCompilers::ASR::is_a<LCompilers::ASR::unit_t>(*asr_new0));
    LCompilers::ASR::TranslationUnit_t *tu
        = LCompilers::ASR::down_cast2<LCompilers::ASR::TranslationUnit_t>(asr_new0);
    LCompilers::fix_external_symbols(*tu, symtab);
    LCOMPILERS_ASSERT(LCompilers::asr_verify(*tu, true, diagnostics));

    std::string asr_new = LCompilers::LFortran::pickle(*asr_new0);

    CHECK(asr_orig == asr_new);
}

void asr_mod(const std::string &src) {
    Allocator al(4*1024);

    LCompilers::LFortran::AST::TranslationUnit_t* ast0;
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::CompilerOptions compiler_options;
    ast0 = TRY(LCompilers::LFortran::parse(al, src, diagnostics));
    LCompilers::ASR::TranslationUnit_t* asr = TRY(LCompilers::LFortran::ast_to_asr(al, *ast0,
        diagnostics, nullptr, false, compiler_options));

    std::string modfile = LCompilers::save_modfile(*asr);
    LCompilers::SymbolTable symtab(nullptr);
    LCompilers::ASR::TranslationUnit_t *asr2 = LCompilers::load_modfile(al,
            modfile, true, symtab);
    LCompilers::fix_external_symbols(*asr2, symtab);
    LCOMPILERS_ASSERT(LCompilers::asr_verify(*asr2, true, diagnostics));

    CHECK(LCompilers::LFortran::pickle(*asr) == LCompilers::LFortran::pickle(*asr2));
}

TEST_CASE("AST Tests") {
    ast_ser("x = 2+2**2");

    ast_ser(R"""(
x = 2+2**2
)""");

    ast_ser("r = a - 2*x");

    ast_ser("r = f(a) - 2*x");

    ast_ser(R"""(
program expr2
implicit none
integer :: x
x = (2+3)*5
print *, x
end program
)""");

    ast_ser(R"""(
integer function f(a, b) result(r)
integer, intent(in) :: a, b
r = a + b
end function
)""");

    ast_ser(R"""(
module modules_05_mod
implicit none
! TODO: the following line does not work yet
!private
integer, parameter, public :: a = 5
integer, parameter :: b = 6
integer, parameter :: c = 7
public :: c
end module


program modules_05
use modules_05_mod, only: a, c
print *, a, c
end program
)""");

    ast_ser(R"""(
program doconcurrentloop_01
implicit none
real, dimension(10000) :: a, b, c
real :: scalar
integer :: i, nsize
scalar = 10
nsize = size(a)
do concurrent (i = 1:nsize)
    a(i) = 5
    b(i) = 5
end do
call triad(a, b, scalar, c)
print *, "End Stream Triad"

contains

    subroutine triad(a, b, scalar, c)
    real, intent(in) :: a(:), b(:), scalar
    real, intent(out) :: c(:)
    integer :: N, i
    N = size(a)
    do concurrent (i = 1:N)
        c(i) = a(i) + scalar * b(i)
    end do
    end subroutine

end program
)""");

}

TEST_CASE("ASR Tests 1") {
    asr_ser(R"""(
program expr2
implicit none
integer :: x
x = (2+3)*5
print *, x
end program
)""");
}

TEST_CASE("ASR Tests 2") {
    asr_ser(R"""(
integer function f(a, b) result(r)
integer, intent(in) :: a, b
r = a + b
end function
)""");
}

TEST_CASE("ASR Tests 3") {
    asr_ser(R"""(
program doconcurrentloop_01
implicit none
real, dimension(10000) :: a, b, c
real :: scalar
integer :: i, nsize
scalar = 10
nsize = size(a)
do concurrent (i = 1:nsize)
    a(i) = 5
    b(i) = 5
end do
call triad(a, b, scalar, c)
print *, "End Stream Triad"

contains

    subroutine triad(a, b, scalar, c)
    real, intent(in) :: a(:), b(:), scalar
    real, intent(out) :: c(:)
    integer :: N, i
    N = size(a)
    do concurrent (i = 1:N)
        c(i) = a(i) + scalar * b(i)
    end do
    end subroutine

end program
)""");
}

TEST_CASE("ASR Tests 4") {
    asr_ser(R"""(
module a
implicit none

contains

subroutine b()
print *, "b()"
end subroutine

end module

program modules_01
use a, only: b
implicit none

call b()

end
)""");
}

TEST_CASE("ASR Tests 5") {
    asr_ser(R"""(
program derived_types_03
implicit none

type :: X
    integer :: i
end type

type(X) :: b

contains

    subroutine Y()
    type :: A
        integer :: i
    end type
    type(A) :: b
    end subroutine

    integer function Z()
    type :: A
        integer :: i
    end type
    type(A) :: b
    Z = 5
    end function

end
)""");

}

TEST_CASE("ASR modfile handling") {
    asr_mod(R"""(
module a
implicit none

contains

subroutine b()
print *, "b()"
end subroutine

end module
)""");

}

TEST_CASE("Topological sorting mod_int") {
    std::map<std::string, std::vector<std::string>> deps;
    // 1 depends on 2
    deps["mod_1"].push_back("mod_2");
    // 3 depends on 1, etc.
    deps["mod_3"].push_back("mod_1");
    deps["mod_2"].push_back("mod_4");
    deps["mod_3"].push_back("mod_4");
    CHECK(LCompilers::ASRUtils::order_deps(deps) == std::vector<std::string>({"mod_4", "mod_2", "mod_1", "mod_3"}));

    deps.clear();
    deps["mod_1"].push_back("mod_2");
    deps["mod_1"].push_back("mod_3");
    deps["mod_2"].push_back("mod_4");
    deps["mod_3"].push_back("mod_4");
    CHECK(LCompilers::ASRUtils::order_deps(deps) == std::vector<std::string>({ "mod_4", "mod_2", "mod_3", "mod_1" }));

    deps.clear();
    deps["mod_1"].push_back("mod_2");
    deps["mod_3"].push_back("mod_1");
    deps["mod_3"].push_back("mod_4");
    deps["mod_4"].push_back("mod_1");
    CHECK(LCompilers::ASRUtils::order_deps(deps) == std::vector<std::string>({ "mod_2", "mod_1", "mod_4", "mod_3" }));
}

TEST_CASE("Topological sorting string") {
    std::map<std::string, std::vector<std::string>> deps;
    // A depends on B
    deps["A"].push_back("B");
    // C depends on A, etc.
    deps["C"].push_back("A");
    deps["B"].push_back("D");
    deps["C"].push_back("D");
    CHECK(LCompilers::ASRUtils::order_deps(deps) == std::vector<std::string>(
                {"D", "B", "A", "C"}));

    deps.clear();
    deps["module_a"].push_back("module_b");
    deps["module_c"].push_back("module_a");
    deps["module_c"].push_back("module_d");
    deps["module_d"].push_back("module_a");
    CHECK(LCompilers::ASRUtils::order_deps(deps) == std::vector<std::string>(
                {"module_b", "module_a", "module_d", "module_c"}));
}
