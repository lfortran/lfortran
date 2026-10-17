#include <tests/doctest.h>
#include <iomanip>
#include <sstream>

#include <iostream>

#include <lfortran/fortran_evaluator.h>
#include <libasr/codegen/evaluator.h>
#include <libasr/alloc.h>
#include <lfortran/ast.h>
#include <libasr/asr.h>
#include <lfortran/parser/parser.h>
#include <lfortran/semantics/ast_to_asr.h>
#include <libasr/asr_verify.h>
#include <libasr/utils.h>
#include <libasr/pass/string_length_arguments.h>
#include <libasr/pass/string_format_array_args.h>

namespace LCompilers::LFortran {


TEST_CASE("Test types") {
    Allocator al(1024*1024);
    Location loc;

    AST::ast_t &a = *AST::make_Num_t(al, loc, 5, nullptr);
    CHECK(AST::is_a<AST::expr_t>(a));
    CHECK(! AST::is_a<AST::decl_stmt_t>(a));

    AST::Num_t &x = *AST::down_cast2<AST::Num_t>(&a);
    CHECK(x.m_n == 5);

}

std::string get_line(std::string str, int n)
{
    std::string line;
    std::stringstream s(str);
    for (int i=0; i < n; i++) {
        std::getline(s, line);
    }
    return line;
}

void populate_span(diag::Span &s, const LocationManager &lm) {
    s.filename = "input.f90";
    lm.pos_to_linecol(lm.output_to_input_pos(s.loc.first, false),
        s.first_line, s.first_column, s.filename);
    lm.pos_to_linecol(lm.output_to_input_pos(s.loc.last, true),
        s.last_line, s.last_column, s.filename);
    std::string input;
    read_file(s.filename, input);
    for (uint32_t i = s.first_line; i <= s.last_line; i++) {
        s.source_code.push_back(get_line(input, i));
    }

}

TEST_CASE("ASR Verify") {
    Allocator al(4*1024);

    std::string src = R"""(
program expr2
implicit none
integer :: x
x = (2+3)*5
print *, x
end program
)""";

    LCompilers::diag::Diagnostics diagnostics;
    CompilerOptions compiler_options;
    compiler_options.lookup_name = true;
    compiler_options.line = "3";
    compiler_options.column = "12";

    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.out_start0 = {};
        fl.in_filename = "input.f90";
        lm.files.push_back(fl);
    }
    FortranEvaluator e(compiler_options);
    AST::TranslationUnit_t* ast = TRY(e.get_ast2(src, lm, diagnostics));
    ASR::TranslationUnit_t* asr = TRY(LFortran::ast_to_asr(al, *ast,
        diagnostics, nullptr, false, compiler_options, lm));

    CHECK(asr_verify(*asr, true, diagnostics)); // Passes

    // Extract the variable "x" from the "x = (2+3)*5" line:
    ASR::Program_t *prog = ASR::down_cast<ASR::Program_t>(asr->m_symtab->get_symbol("expr2"));
    ASR::Assignment_t *a = ASR::down_cast<ASR::Assignment_t>(prog->m_body[0]);
    ASR::Var_t *v = ASR::down_cast<ASR::Var_t>(a->m_target);
    ASR::symbol_t* sym = v->m_v;
    std::vector<diag::Span> spans = diag::Label("", {sym->base.loc}).spans;
    for( auto it: spans ) {
        populate_span(it, lm);
        CHECK(it.first_line == 4);
        CHECK(it.first_column == 12);
        CHECK(it.last_line == 4);
        CHECK(it.last_column == 12);
    }
    v->m_v = &(prog->base); // Assign the wrong symbol to Var_t::m_v

    // This will be caught by the verifier
    CHECK(!asr_verify(*asr, true, diagnostics));
}

TEST_CASE("ASR Verify INTENT(OUT) actual argument") {
    Allocator al(4*1024);

    std::string src = R"""(
program p
implicit none
integer :: x
call s(x)
contains
subroutine s(y)
    integer, intent(out) :: y
end subroutine
end program
)""";

    LCompilers::diag::Diagnostics diagnostics;
    CompilerOptions compiler_options;
    compiler_options.lookup_name = true;

    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.out_start0 = {};
        fl.in_filename = "input.f90";
        lm.files.push_back(fl);
    }

    FortranEvaluator e(compiler_options);
    AST::TranslationUnit_t* ast = TRY(e.get_ast2(src, lm, diagnostics));
    ASR::TranslationUnit_t* asr = TRY(LFortran::ast_to_asr(al, *ast,
        diagnostics, nullptr, false, compiler_options, lm));

    CHECK(asr_verify(*asr, true, diagnostics)); // Valid ASR

    // Mutate a valid call so that INTENT(OUT) receives a constant expression.
    ASR::Program_t *prog = ASR::down_cast<ASR::Program_t>(asr->m_symtab->get_symbol("p"));
    ASR::SubroutineCall_t *call_stmt = ASR::down_cast<ASR::SubroutineCall_t>(prog->m_body[0]);
    ASR::Var_t* arg_var = ASR::down_cast<ASR::Var_t>(call_stmt->m_args[0].m_value);
    ASR::Variable_t* variable = ASR::down_cast<ASR::Variable_t>(arg_var->m_v);
    call_stmt->m_args[0].m_value = ASR::down_cast<ASR::expr_t>(
        ASR::make_IntegerConstant_t(al, call_stmt->m_args[0].m_value->base.loc, 1, variable->m_type));

    CHECK(!asr_verify(*asr, true, diagnostics));
}

TEST_CASE("ASR Verify INTENT(INOUT) ArrayPhysicalCast argument path") {
    Allocator al(4*1024);

    std::string src = R"""(
program arraywrite
implicit none
integer, allocatable :: arr(:,:)
call ss(arr(1:2,2:3))
contains
subroutine ss(s)
    integer, intent(inout) :: s(2)
end subroutine
end program arraywrite
)""";

    LCompilers::diag::Diagnostics diagnostics;
    CompilerOptions compiler_options;
    compiler_options.lookup_name = true;

    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.out_start0 = {};
        fl.in_filename = "input.f90";
        lm.files.push_back(fl);
    }

    FortranEvaluator e(compiler_options);
    AST::TranslationUnit_t* ast = TRY(e.get_ast2(src, lm, diagnostics));
    ASR::TranslationUnit_t* asr = TRY(LFortran::ast_to_asr(al, *ast,
        diagnostics, nullptr, false, compiler_options, lm));

    CHECK(asr_verify(*asr, true, diagnostics)); // Valid ASR

    ASR::Program_t *prog = ASR::down_cast<ASR::Program_t>(asr->m_symtab->get_symbol("arraywrite"));
    ASR::SubroutineCall_t *call_stmt = ASR::down_cast<ASR::SubroutineCall_t>(prog->m_body[0]);
    CHECK(call_stmt->n_args == 1);
    CHECK(ASR::is_a<ASR::ArrayPhysicalCast_t>(*call_stmt->m_args[0].m_value));

    // Mutate cast payload to a non-variable expression.
    ASR::ArrayPhysicalCast_t* cast_arg = ASR::down_cast<ASR::ArrayPhysicalCast_t>(call_stmt->m_args[0].m_value);
    ASR::ttype_t* int_type = ASR::down_cast<ASR::ttype_t>(
        ASR::make_Integer_t(al, cast_arg->m_arg->base.loc, 4));
    cast_arg->m_arg = ASR::down_cast<ASR::expr_t>(
        ASR::make_IntegerConstant_t(al, cast_arg->m_arg->base.loc, 1, int_type));

    CHECK(!asr_verify(*asr, true, diagnostics));
}

TEST_CASE("ASR Verify INTENT(OUT) StringPhysicalCast argument path") {
    Allocator al(4*1024);

    std::string src = R"""(
program p
implicit none
character(1), allocatable :: str
call ss(str)
contains
subroutine ss(s) bind(c)
    character(1), intent(out) :: s
end subroutine
end program p
)""";

    LCompilers::diag::Diagnostics diagnostics;
    CompilerOptions compiler_options;
    compiler_options.lookup_name = true;

    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.out_start0 = {};
        fl.in_filename = "input.f90";
        lm.files.push_back(fl);
    }

    FortranEvaluator e(compiler_options);
    AST::TranslationUnit_t* ast = TRY(e.get_ast2(src, lm, diagnostics));
    ASR::TranslationUnit_t* asr = TRY(LFortran::ast_to_asr(al, *ast,
        diagnostics, nullptr, false, compiler_options, lm));

    CHECK(asr_verify(*asr, true, diagnostics)); // Valid ASR

    ASR::Program_t *prog = ASR::down_cast<ASR::Program_t>(asr->m_symtab->get_symbol("p"));
    ASR::SubroutineCall_t *call_stmt = ASR::down_cast<ASR::SubroutineCall_t>(prog->m_body[0]);
    REQUIRE(ASR::is_a<ASR::StringPhysicalCast_t>(*call_stmt->m_args[0].m_value));
    ASR::StringPhysicalCast_t* cast_arg =
        ASR::down_cast<ASR::StringPhysicalCast_t>(call_stmt->m_args[0].m_value);
    CHECK(asr_verify(*asr, true, diagnostics));

    cast_arg->m_old = ASR::string_physical_typeType::CChar;
    CHECK(!asr_verify(*asr, true, diagnostics));
    diagnostics.diagnostics.clear();
    cast_arg->m_old = ASR::string_physical_typeType::DescriptorString;

    cast_arg->m_new = ASR::string_physical_typeType::DescriptorString;
    CHECK(!asr_verify(*asr, true, diagnostics));
    diagnostics.diagnostics.clear();
    cast_arg->m_new = ASR::string_physical_typeType::CChar;
    CHECK(asr_verify(*asr, true, diagnostics));

    // Mutate cast payload to a non-variable expression.
    ASR::expr_t* original_arg = cast_arg->m_arg;
    ASR::ttype_t* int_type = ASR::down_cast<ASR::ttype_t>(
        ASR::make_Integer_t(al, original_arg->base.loc, 4));
    cast_arg->m_arg = ASR::down_cast<ASR::expr_t>(
        ASR::make_IntegerConstant_t(al, original_arg->base.loc, 1, int_type));

    CHECK(!asr_verify(*asr, true, diagnostics));
}

TEST_CASE("ASR Verify pass method call requires m_dt") {
    Allocator al(16*1024);

    std::string src = R"""(
module m
  type :: t
  contains
    procedure :: set_value_pass
    generic :: set_value => set_value_pass
  end type
contains
  subroutine set_value_pass(this, value)
    class(t), intent(inout) :: this
    integer, intent(in) :: value
  end subroutine
end module

program p
  use m
  type(t) :: obj
  call obj%set_value(1)
end program
)""";

    LCompilers::diag::Diagnostics diagnostics;
    CompilerOptions compiler_options;
    compiler_options.lookup_name = true;

    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.out_start0 = {};
        fl.in_filename = "input.f90";
        lm.files.push_back(fl);
    }

    FortranEvaluator e(compiler_options);
    AST::TranslationUnit_t* ast = TRY(e.get_ast2(src, lm, diagnostics));
    ASR::TranslationUnit_t* asr = TRY(LFortran::ast_to_asr(al, *ast,
        diagnostics, nullptr, false, compiler_options, lm));

    CHECK(asr_verify(*asr, true, diagnostics)); // Valid ASR

    ASR::Program_t *prog = ASR::down_cast<ASR::Program_t>(asr->m_symtab->get_symbol("p"));
    ASR::SubroutineCall_t *call_stmt = ASR::down_cast<ASR::SubroutineCall_t>(prog->m_body[0]);
    CHECK(call_stmt->m_dt != nullptr);

    call_stmt->m_dt = nullptr;
    CHECK(!asr_verify(*asr, true, diagnostics));
}

TEST_CASE("ASR Verify hidden string length arguments") {
    Allocator al(64*1024);

    std::string src = R"""(
program p
implicit none
character(len=5) :: x
call s(x, 2)
contains
subroutine s(a, n)
    character(len=*), intent(inout) :: a
    integer, intent(in) :: n
    a(n:n) = 'z'
end subroutine
end program
)""";

    LCompilers::diag::Diagnostics diagnostics;
    CompilerOptions compiler_options;

    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.out_start0 = {};
        fl.in_filename = "input.f90";
        lm.files.push_back(fl);
    }

    FortranEvaluator e(compiler_options);
    AST::TranslationUnit_t* ast = TRY(e.get_ast2(src, lm, diagnostics));
    ASR::TranslationUnit_t* asr = TRY(LFortran::ast_to_asr(al, *ast,
        diagnostics, nullptr, false, compiler_options, lm));

    ASRVerifyOptions after_pass;
    after_pass.string_length_arguments = true;
    CHECK(asr_verify(*asr, true, diagnostics));
    // Before the pass, `a` has no hidden length.
    CHECK(!asr_verify(*asr, after_pass, diagnostics));
    CHECK(diagnostics.diagnostics.back().code ==
        "asr.verify.function.hidden_string_length_missing");
    diagnostics.diagnostics.clear();

    PassOptions pass_options;
    pass_string_length_arguments(al, *asr, pass_options);
    CHECK(asr_verify(*asr, after_pass, diagnostics));

    ASR::Program_t *prog = ASR::down_cast<ASR::Program_t>(
        asr->m_symtab->get_symbol("p"));
    ASR::Function_t *s = ASR::down_cast<ASR::Function_t>(
        prog->m_symtab->get_symbol("s"));
    CHECK(s->n_args == 3);
    ASR::SubroutineCall_t *call_stmt = ASR::down_cast<ASR::SubroutineCall_t>(
        prog->m_body[0]);
    REQUIRE(call_stmt->n_args == 3);
    ASR::IntegerConstant_t *len = ASR::down_cast<ASR::IntegerConstant_t>(
        call_stmt->m_args[2].m_value);
    CHECK(len->m_n == 5);

    // A call that does not pass the hidden length is caught.
    call_stmt->n_args = 2;
    CHECK(!asr_verify(*asr, after_pass, diagnostics));
}

TEST_CASE("ASR Verify array arguments of StringFormat") {
    Allocator al(64*1024);

    std::string src = R"""(
program p
implicit none
integer, allocatable :: x(:)
integer, pointer :: y(:)
allocate(x(3))
print *, x, y
end program
)""";

    LCompilers::diag::Diagnostics diagnostics;
    CompilerOptions compiler_options;

    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.out_start0 = {};
        fl.in_filename = "input.f90";
        lm.files.push_back(fl);
    }

    FortranEvaluator e(compiler_options);
    AST::TranslationUnit_t* ast = TRY(e.get_ast2(src, lm, diagnostics));
    ASR::TranslationUnit_t* asr = TRY(LFortran::ast_to_asr(al, *ast,
        diagnostics, nullptr, false, compiler_options, lm));

    ASRVerifyOptions after_pass;
    after_pass.string_format_array_args = true;
    CHECK(asr_verify(*asr, true, diagnostics));
    // Before the pass, `x` and `y` are descriptors.
    CHECK(!asr_verify(*asr, after_pass, diagnostics));
    CHECK(diagnostics.diagnostics.back().code ==
        "asr.verify.string_format.array_argument_not_pointer_array");
    diagnostics.diagnostics.clear();

    PassOptions pass_options;
    pass_string_format_array_args(al, *asr, pass_options);
    CHECK(asr_verify(*asr, after_pass, diagnostics));

    ASR::Program_t *prog = ASR::down_cast<ASR::Program_t>(
        asr->m_symtab->get_symbol("p"));
    ASR::Print_t *print = nullptr;
    for (size_t i = 0; i < prog->n_body; i++) {
        if (ASR::is_a<ASR::Print_t>(*prog->m_body[i])) {
            print = ASR::down_cast<ASR::Print_t>(prog->m_body[i]);
        }
    }
    REQUIRE(print != nullptr);
    ASR::StringFormat_t *fmt = ASR::down_cast<ASR::StringFormat_t>(print->m_text);
    REQUIRE(fmt->n_args == 2);
    for (size_t i = 0; i < fmt->n_args; i++) {
        REQUIRE(ASR::is_a<ASR::ArrayPhysicalCast_t>(*fmt->m_args[i]));
        ASR::ArrayPhysicalCast_t *cast = ASR::down_cast<ASR::ArrayPhysicalCast_t>(
            fmt->m_args[i]);
        CHECK(cast->m_old == ASR::array_physical_typeType::DescriptorArray);
        CHECK(cast->m_new == ASR::array_physical_typeType::PointerArray);
    }
    // The allocatable `x` is cast as it is, the pointer `y` through a
    // contiguous temporary.
    CHECK(ASR::is_a<ASR::Var_t>(*ASR::down_cast<ASR::ArrayPhysicalCast_t>(
        fmt->m_args[0])->m_arg));
    CHECK(ASR::down_cast<ASR::Var_t>(ASR::down_cast<ASR::ArrayPhysicalCast_t>(
        fmt->m_args[0])->m_arg)->m_v == prog->m_symtab->get_symbol("x"));
    CHECK(ASR::down_cast<ASR::Var_t>(ASR::down_cast<ASR::ArrayPhysicalCast_t>(
        fmt->m_args[1])->m_arg)->m_v != prog->m_symtab->get_symbol("y"));

    // A descriptor argument is caught again.
    fmt->m_args[0] = ASR::down_cast<ASR::ArrayPhysicalCast_t>(fmt->m_args[0])->m_arg;
    CHECK(!asr_verify(*asr, after_pass, diagnostics));
}

TEST_CASE("Variable Location") {
    Allocator al(4*1024);

    std::string src = R"""(
program expr2
implicit none
integer :: x
x = (2+3)*5
print *, x
end program
)""";

    LCompilers::diag::Diagnostics diagnostics;
    CompilerOptions compiler_options;
    compiler_options.lookup_name = true;
    compiler_options.line = "3";
    compiler_options.column = "12";

    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.out_start0 = {};
        fl.in_filename = "input.f90";
        lm.files.push_back(fl);
    }
    FortranEvaluator e(compiler_options);
    LCompilers::Result<LCompilers::ASR::TranslationUnit_t*>
        r = e.get_asr2(src, lm, diagnostics);
    uint32_t input_pos = LCompilers::LFortran::source_offset(
        lm.linecol_to_pos(2, 12));
    uint32_t output_pos = lm.input_to_output_pos(input_pos, false);
    CHECK(lm.output_to_input_pos(output_pos, false) == input_pos);
    ASR::asr_t* asr2 = e.handle_lookup_name(r.result, output_pos);
    std::vector<diag::Span> spans2 = diag::Label("", {asr2->loc}).spans;
    for( auto it: spans2 ) {
        populate_span(it, lm);
        CHECK(it.first_line == 2);
        CHECK(it.first_column == 1);
        CHECK(it.last_line == 7);
        CHECK(it.last_column == 11);
    }
}

TEST_CASE("Variable Location beyond 65535") {
    Allocator al(4*1024);

    // A long first comment line puts the program past position 65535
    std::string src = "!" + std::string(70000, 'x') + R"""(
program expr2
implicit none
integer :: x
x = (2+3)*5
print *, x
end program
)""";

    LCompilers::diag::Diagnostics diagnostics;
    CompilerOptions compiler_options;
    compiler_options.lookup_name = true;

    LCompilers::LocationManager lm;
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.out_start0 = {};
        fl.in_filename = "input.f90";
        lm.files.push_back(fl);
    }
    FortranEvaluator e(compiler_options);
    LCompilers::Result<LCompilers::ASR::TranslationUnit_t*>
        r = e.get_asr2(src, lm, diagnostics);
    REQUIRE(r.ok);
    uint32_t input_pos = LCompilers::LFortran::source_offset(
        lm.linecol_to_pos(2, 12));
    uint32_t output_pos = lm.input_to_output_pos(input_pos, false);
    CHECK(output_pos > 65535);
    ASR::asr_t* asr2 = e.handle_lookup_name(r.result, output_pos);
    REQUIRE(asr2 != nullptr);
    std::vector<diag::Span> spans2 = diag::Label("", {asr2->loc}).spans;
    for( auto it: spans2 ) {
        populate_span(it, lm);
        CHECK(it.first_line == 2);
        CHECK(it.first_column == 1);
        CHECK(it.last_line == 7);
        CHECK(it.last_column == 11);
    }
}


} // namespace LCompilers::LFortran
