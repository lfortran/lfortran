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

TEST_CASE("Fortran output separates trait inspection from compilation") {
    const std::string declarations = R"(
module trait_fortran_output_m
abstract interface :: IValue
    function value() result(r)
        integer :: r
    end function
end interface
type :: Payload
    integer :: n
end type
implements IValue :: Payload
    procedure, pass :: value => read_value
end implements
contains
function read_value(self) result(r)
    type(Payload), intent(in) :: self
    integer :: r
    r = self%n
end function
)";
    const std::string runtime_consumer = R"(
function observe(object) result(r)
    class(IValue), intent(in) :: object
    integer :: r
    r = object%value()
end function
)";
    for (bool runtime : {false, true}) {
        for (bool for_compilation : {false, true}) {
            std::string source = declarations +
                (runtime ? runtime_consumer : "") + "end module\n";
            CompilerOptions options;
            FortranEvaluator evaluator(options);
            LocationManager lm;
            LocationManager::FileLocations file;
            file.in_filename = "trait_fortran_output.f90";
            lm.files.push_back(file);
            lm.file_ends.push_back(source.size());
            diag::Diagnostics diagnostics;
            PassManager passes;
            auto output = evaluator.get_fortran(
                source, lm, diagnostics, passes, for_compilation);
            INFO(diagnostics.render2());
            if (runtime && for_compilation) {
                CHECK_FALSE(output.ok);
                CHECK(diagnostics.has_error());
                REQUIRE(!diagnostics.diagnostics.empty());
                CHECK(diagnostics.diagnostics.back().message ==
                    "runtime trait dispatch is not implemented by the fortran backend");
            } else {
                REQUIRE(output.ok);
                CHECK((output.result.find("abstract interface :: ivalue") !=
                    std::string::npos) == !for_compilation);
                CHECK((output.result.find("implements ivalue :: payload") !=
                    std::string::npos) == !for_compilation);
            }
        }
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
character(2), allocatable :: str
call ss(str)
contains
subroutine ss(s)
    character(2), intent(out) :: s
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
    ASR::expr_t* original_arg = call_stmt->m_args[0].m_value;

    ASR::ttype_t* cast_type = ASR::down_cast<ASR::ttype_t>(
        ASR::make_String_t(al, original_arg->base.loc, 1, nullptr,
            ASR::string_length_kindType::ImplicitLength,
            ASR::string_physical_typeType::DescriptorString));
    call_stmt->m_args[0].m_value = ASR::down_cast<ASR::expr_t>(
        ASR::make_StringPhysicalCast_t(al, original_arg->base.loc, original_arg,
            ASR::string_physical_typeType::DescriptorString,
            ASR::string_physical_typeType::CChar, cast_type, nullptr));

    CHECK(asr_verify(*asr, true, diagnostics));

    // Mutate cast payload to a non-variable expression.
    ASR::StringPhysicalCast_t* cast_arg =
        ASR::down_cast<ASR::StringPhysicalCast_t>(call_stmt->m_args[0].m_value);
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
    uint64_t input_pos = lm.linecol_to_pos(2, 12);
    uint64_t output_pos = lm.input_to_output_pos(input_pos, false);
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


} // namespace LCompilers::LFortran
