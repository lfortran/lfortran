#include <tests/doctest.h>
#include <iostream>

#include <libasr/bwriter.h>
#include <libasr/serialization.h>
#include <lfortran/ast_serialization.h>
#include <libasr/modfile.h>
#include <lfortran/pickle.h>
#include <libasr/pickle.h>
#include <lfortran/parser/parser.h>
#include <lfortran/semantics/ast_to_asr.h>
#include <libasr/asr_utils.h>
#include <libasr/asr_side_effect.h>
#include <libasr/asr_verify.h>
#include <libasr/asr_text.h>
#include <libasr/pass/pass_utils.h>
#include <libasr/pass/function_result_scope.h>
#include <libasr/pass/create_subroutine_from_function.h>
#include <libasr/pass/intent_out_deallocate.h>
#include <libasr/utils.h>
#include <lfortran/ast_to_src.h>

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
    LCompilers::CompilerOptions co;
    co.interactive = true;
    result = TRY(LCompilers::LFortran::parse(al, src, diagnostics, co));
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
    ast0 = TRY(LCompilers::LFortran::parse(al, src, diagnostics, compiler_options));
    LCompilers::LocationManager lm;
    LCompilers::ASR::TranslationUnit_t* asr = TRY(LCompilers::LFortran::ast_to_asr(al, *ast0,
        diagnostics, nullptr, false, compiler_options, lm));

    std::string asr_orig = LCompilers::pickle(*asr);
    std::string binary = LCompilers::serialize(*asr);

    LCompilers::ASR::asr_t *asr_new0;
    LCompilers::SymbolTable symtab(nullptr);
    asr_new0 = LCompilers::deserialize_asr(al, binary, true, symtab, 0);
    CHECK(LCompilers::ASR::is_a<LCompilers::ASR::unit_t>(*asr_new0));
    LCompilers::ASR::TranslationUnit_t *tu
        = LCompilers::ASR::down_cast2<LCompilers::ASR::TranslationUnit_t>(asr_new0);
    fix_external_symbols(*tu, symtab);
    LCOMPILERS_ASSERT(LCompilers::asr_verify(*tu, true, diagnostics));

    std::string asr_new = LCompilers::pickle(*asr_new0);

    CHECK(asr_orig == asr_new);
}

void asr_mod(const std::string &src, const std::string &module_name = "") {
    Allocator al(4*1024);

    LCompilers::LFortran::AST::TranslationUnit_t* ast0;
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::CompilerOptions compiler_options;
    ast0 = TRY(LCompilers::LFortran::parse(al, src, diagnostics, compiler_options));
    LCompilers::LocationManager lm;
    lm.file_ends.push_back(0);
    LCompilers::LocationManager::FileLocations file;
    file.out_start.push_back(0); file.in_start.push_back(0); file.in_newlines.push_back(0);
    file.in_filename = "test"; file.current_line = 1; file.preprocessor = false; file.out_start0.push_back(0);
    file.in_start0.push_back(0); file.in_size0.push_back(0); file.interval_type0.push_back(0);
    file.in_newlines0.push_back(0);
    lm.files.push_back(file);
    LCompilers::ASR::TranslationUnit_t* asr = TRY(LCompilers::LFortran::ast_to_asr(al, *ast0,
        diagnostics, nullptr, false, compiler_options, lm));

    LCompilers::SymbolTable symtab(nullptr);
    LCompilers::SymbolTable *external_symtab = &symtab;
    LCompilers::ASR::Module_t *module = nullptr;
    if (!module_name.empty()) {
        external_symtab = asr->m_symtab;
        module = LCompilers::ASR::down_cast<LCompilers::ASR::Module_t>(
            external_symtab->get_symbol(module_name));
        auto *module_scope = al.make_new<LCompilers::SymbolTable>(nullptr);
        module_scope->add_symbol(module_name, &module->base);
        module->m_symtab->parent = module_scope;
        asr = LCompilers::ASR::down_cast2<LCompilers::ASR::TranslationUnit_t>(
            LCompilers::ASR::make_TranslationUnit_t(
                al, module->base.base.loc, module_scope, nullptr, 0, nullptr));
    }
    std::string original = LCompilers::pickle(*asr);
    std::string modfile = LCompilers::save_modfile(*asr, lm);
    if (module) module->m_symtab->parent = external_symtab;
    LCompilers::Result<LCompilers::ASR::TranslationUnit_t*, LCompilers::ErrorMessage> res
        = LCompilers::load_modfile(al, modfile, true, *external_symtab, lm);
    CHECK(res.ok);
    LCompilers::ASR::TranslationUnit_t* asr2 = res.result;
    fix_external_symbols(*asr2, *external_symtab);
    LCOMPILERS_ASSERT(LCompilers::asr_verify(*asr2, true, diagnostics));

    CHECK(original == LCompilers::pickle(*asr2));
}

static const std::string numeric_trait_source = R"(
module numeric_contracts
implicit none
integer, parameter :: rk = 8
abstract interface :: INumeric
    integer | real(rk)
end interface
abstract interface :: IComplex
    complex(rk)
end interface
type :: Box
    integer :: value
end type
contains
function first{INumeric :: T}(x) result(r)
    type(T), intent(in) :: x
    type(T) :: r
    r = second{T}(x)
end function
function second{INumeric :: U}(x) result(r)
    type(U), intent(in) :: x
    type(U) :: r
    r = helper(x)
end function
function helper{INumeric :: V}(x) result(r)
    type(V), intent(in) :: x
    type(V) :: r
    r = x + V(1)
end function
function unused_arithmetic{INumeric :: T}(x, y, n) result(r)
    type(T), intent(in) :: x, y
    integer, intent(in) :: n
    type(T) :: r
    r = (x + y) * T(n) - x / y
end function
function unused_compare{INumeric :: T}(x, y) result(r)
    type(T), intent(in) :: x, y
    logical :: r
    r = x < y .or. x > y
end function
function unused_equal{IComplex :: T}(x, y) result(r)
    type(T), intent(in) :: x, y
    logical :: r
    r = x == y
end function
function unused_complex_cast{IComplex :: T}(n) result(r)
    integer, intent(in) :: n
    type(T) :: r
    r = T(n)
end function
function numeric_sum{INumeric :: T}(x) result(r)
    type(T), intent(in) :: x(:)
    type(T) :: r
    integer :: i
    r = T(0)
    do i = 1, size(x)
        r = r + x(i)
    end do
end function
end module
module numeric_other
implicit none
abstract interface :: INumeric
    integer | integer(4)
end interface
end module
program numeric_client
use numeric_contracts, only: first, renamed => first, RootNumeric => INumeric, numeric_sum
use numeric_other, only: OtherNumeric => INumeric
implicit none
integer :: i, j
real(8) :: a, b
i = first(2)
j = renamed{integer}(3)
a = first(2.d0)
b = renamed{real(8)}(3.d0)
i = numeric_sum([1, 2, 3])
a = numeric_sum([1.d0, 2.d0, 3.d0])
end program
)";

static LCompilers::ASR::TraitConstraint_t *numeric_constraint(
        LCompilers::ASR::Module_t *module, const std::string &name) {
    namespace ASR = LCompilers::ASR;
    auto *generic = ASR::down_cast<ASR::Template_t>(module->m_symtab->get_symbol(name));
    for (const auto &entry : generic->m_symtab->get_scope()) {
        if (ASR::is_a<ASR::TraitConstraint_t>(*entry.second)) {
            return ASR::down_cast<ASR::TraitConstraint_t>(entry.second);
        }
    }
    return nullptr;
}

static const std::string inline_numeric_source = R"(
module inline_numeric_m
implicit none
private
public :: shift, walk
integer, parameter :: rk = 8
contains
function shift{integer | real(rk) :: T}(x, n) result(r)
    type(T), intent(in) :: x
    integer, intent(in) :: n
    type(T) :: r
    r = x + T(n)
end function
recursive function walk{integer | real(rk) :: T}(x, n) result(r)
    type(T), intent(in) :: x
    integer, intent(in) :: n
    type(T) :: r
    if (n == 0) then
        r = x
    else if (n == 1) then
        r = walk{T}(x + T(1), n-1)
    else
        r = walk(x + T(1), n-1)
    end if
end function
subroutine unused{integer(4) :: U, integer(4) :: V}(x, y)
    type(U), intent(inout) :: x
    type(V), intent(inout) :: y
    x = x + U(1)
    y = y + V(2)
end subroutine
end module
module inline_other_m
implicit none
contains
function shift{integer | real(8) :: T}(x, n) result(r)
    type(T), intent(in) :: x
    integer, intent(in) :: n
    type(T) :: r
    r = x + T(n) + T(n)
end function
end module
module inline_facade_m
use inline_numeric_m, only: renamed => shift, again => shift, walk
implicit none
private
public :: renamed, again, walk
end module
program inline_client
use inline_numeric_m, only: shift
use inline_facade_m, only: renamed, again, walk
use inline_other_m, only: other => shift
implicit none
integer :: i, j, k
real(8) :: a, b
i = shift(1, 2)
j = renamed{integer}(1, 2)
k = other(1, 2)
a = again(0.d0, 16777217)
b = shift{real(8)}(0.d0, 16777217)
i = walk(1, 3)
j = walk{integer}(1, 3)
a = walk(1.d0, 3)
end program
)";

TEST_CASE("Inline numeric constraints preserve owned identities and roundtrips") {
    namespace ASR = LCompilers::ASR;
    namespace ASRUtils = LCompilers::ASRUtils;
    ast_ser(inline_numeric_source);
    asr_ser(inline_numeric_source);
    asr_mod(inline_numeric_source, "inline_numeric_m");
    asr_mod(inline_numeric_source, "inline_facade_m");

    Allocator al(1024 * 1024);
    LCompilers::CompilerOptions options;
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::LocationManager lm;
    auto parsed = LCompilers::LFortran::parse(al, inline_numeric_source, diagnostics, options);
    REQUIRE(parsed.ok);
    auto result = LCompilers::LFortran::ast_to_asr(
        al, *parsed.result, diagnostics, nullptr, false, options, lm);
    INFO(diagnostics.render2());
    REQUIRE(result.ok);

    auto check_identity = [&](ASR::TranslationUnit_t *unit) {
        CHECK(LCompilers::asr_verify(*unit, true, diagnostics));
        auto *module = ASR::down_cast<ASR::Module_t>(
            unit->m_symtab->get_symbol("inline_numeric_m"));
        auto *other = ASR::down_cast<ASR::Module_t>(
            unit->m_symtab->get_symbol("inline_other_m"));
        auto *constraint = numeric_constraint(module, "shift");
        auto *other_constraint = numeric_constraint(other, "shift");
        auto *trait = ASR::down_cast<ASR::Trait_t>(constraint->m_trait);
        CHECK(trait != ASR::down_cast<ASR::Trait_t>(other_constraint->m_trait));
        CHECK(trait->m_symtab->parent == constraint->m_parent_symtab);
        CHECK(trait->m_access == ASR::Private);
        CHECK(trait->m_kind == ASR::IntrinsicTypeSet);
        CHECK(trait->n_member_types == 2);
        CHECK(ASRUtils::get_asr_owner(&trait->base) == module->m_symtab->get_symbol("shift"));
        for (const char *name : {"shift", "walk", "unused"}) {
            auto *c = numeric_constraint(module, name);
            REQUIRE(c);
            CHECK(c->n_intrinsic_requirements == 2);
            for (size_t i = 0; i < c->n_intrinsic_requirements; i++) {
                CHECK(c->m_intrinsic_requirements[i].n_witnesses ==
                    ASR::down_cast<ASR::Trait_t>(c->m_trait)->n_member_types);
            }
        }
        auto *unused = ASR::down_cast<ASR::Template_t>(module->m_symtab->get_symbol("unused"));
        std::set<ASR::symbol_t*> binder_traits;
        for (const auto &entry : unused->m_symtab->get_scope()) {
            if (ASR::is_a<ASR::TraitConstraint_t>(*entry.second)) {
                binder_traits.insert(ASR::down_cast<ASR::TraitConstraint_t>(
                    entry.second)->m_trait);
            }
        }
        CHECK(binder_traits.size() == 2);
        auto *client = ASR::down_cast<ASR::Program_t>(
            unit->m_symtab->get_symbol("inline_client"));
        auto call = [&](size_t i) {
            return ASR::down_cast<ASR::FunctionCall_t>(
                ASR::down_cast<ASR::Assignment_t>(client->m_body[i])->m_value);
        };
        auto callee = [&](size_t i) {
            return ASR::down_cast<ASR::Function_t>(
                ASRUtils::symbol_get_past_external(call(i)->m_name));
        };
        CHECK(callee(0) == callee(1));
        CHECK(callee(0) != callee(2));
        CHECK(callee(3) == callee(4));
        CHECK(callee(0) != callee(3));
        CHECK(callee(5) == callee(6));
        CHECK(callee(5) != callee(7));
        CHECK(ASR::is_a<ASR::Integer_t>(*call(0)->m_type));
        CHECK(ASR::is_a<ASR::Real_t>(*call(3)->m_type));
        CHECK(ASRUtils::extract_kind_from_ttype_t(call(3)->m_type) == 8);
        struct Calls : ASR::BaseWalkVisitor<Calls> {
            ASR::symbol_t *target;
            size_t count = 0;
            void visit_FunctionCall(const ASR::FunctionCall_t &x) {
                if (ASRUtils::symbol_get_past_external(x.m_name) == target) count++;
                ASR::BaseWalkVisitor<Calls>::visit_FunctionCall(x);
            }
        };
        auto *generic = ASR::down_cast<ASR::Template_t>(
            module->m_symtab->get_symbol("walk"));
        auto *original = ASR::down_cast<ASR::Function_t>(
            generic->m_symtab->get_symbol("walk"));
        for (auto *function : {original, callee(5), callee(7)}) {
            Calls calls;
            calls.target = &function->base;
            for (size_t i = 0; i < function->n_body; i++) {
                calls.visit_stmt(*function->m_body[i]);
            }
            CHECK(calls.count == 2);
        }
        auto *facade = ASR::down_cast<ASR::Module_t>(
            unit->m_symtab->get_symbol("inline_facade_m"));
        CHECK(ASRUtils::symbol_get_past_external(facade->m_symtab->get_symbol("renamed")) ==
            module->m_symtab->get_symbol("shift"));
        CHECK(ASRUtils::symbol_get_past_external(client->m_symtab->get_symbol("again")) ==
            module->m_symtab->get_symbol("shift"));
    };
    check_identity(result.result);
    LCompilers::SymbolTable symtab(nullptr);
    auto *copy = ASR::down_cast2<ASR::TranslationUnit_t>(LCompilers::deserialize_asr(
        al, LCompilers::serialize(*result.result), true, symtab, 0));
    fix_external_symbols(*copy, symtab);
    check_identity(copy);
    for (auto form : {LCompilers::ASRTextForm::Named, LCompilers::ASRTextForm::Positional}) {
        LCompilers::ASRTextOptions text_options;
        text_options.form = form;
        auto text = LCompilers::asr_to_text(*result.result, text_options);
        auto loaded = LCompilers::asr_from_text(al, text, "inline.asr", lm, diagnostics);
        REQUIRE(loaded.ok);
        check_identity(loaded.result);
        CHECK(text == LCompilers::asr_to_text(*loaded.result, text_options));
    }
    auto printed = LCompilers::LFortran::ast_to_src(*parsed.result);
    CHECK(printed.find("{integer | real(rk) :: T}") != std::string::npos);
    CHECK(printed.find("{integer(4) :: U, integer(4) :: V}") != std::string::npos);
    auto reparsed = LCompilers::LFortran::parse(al, printed, diagnostics, options);
    REQUIRE(reparsed.ok);
    auto rebuilt = LCompilers::LFortran::ast_to_asr(
        al, *reparsed.result, diagnostics, nullptr, false, options, lm);
    REQUIRE(rebuilt.ok);
    check_identity(rebuilt.result);
    CHECK(LCompilers::asr_to_text(*result.result) == LCompilers::asr_to_text(*rebuilt.result));
}

TEST_CASE("Inline numeric constraints verify private single-binder ownership") {
    namespace ASR = LCompilers::ASR;
    const std::string source = R"(
module inline_owner
implicit none
contains
function identity{integer(4) :: T}(x) result(r)
    type(T), intent(in) :: x
    type(T) :: r
    r = x
end function
end module
)";
    Allocator al(1024 * 1024);
    LCompilers::CompilerOptions options;
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::LocationManager lm;
    auto parsed = LCompilers::LFortran::parse(al, source, diagnostics, options);
    REQUIRE(parsed.ok);
    auto result = LCompilers::LFortran::ast_to_asr(
        al, *parsed.result, diagnostics, nullptr, false, options, lm);
    REQUIRE(result.ok);
    auto check = [&](ASR::TranslationUnit_t *unit) {
        auto *module = ASR::down_cast<ASR::Module_t>(unit->m_symtab->get_symbol("inline_owner"));
        auto *constraint = numeric_constraint(module, "identity");
        auto *trait = ASR::down_cast<ASR::Trait_t>(constraint->m_trait);
        auto rejected = [&](const std::string &code) {
            LCompilers::diag::Diagnostics errors;
            CHECK_FALSE(LCompilers::asr_verify(*unit, true, errors));
            REQUIRE(!errors.diagnostics.empty());
            INFO(errors.render2());
            CHECK(errors.diagnostics.back().code == code);
        };
        trait->m_access = ASR::Public;
        rejected("asr.verify.trait.inline_is_private_type_set");
        trait->m_access = ASR::Private;
        trait->m_kind = ASR::UniversalTrait;
        rejected("asr.verify.trait.inline_is_private_type_set");
        trait->m_kind = ASR::IntrinsicTypeSet;
        auto *scope = constraint->m_parent_symtab;
        scope->erase_symbol(constraint->m_name);
        rejected("asr.verify.trait.inline_has_one_binder");
        scope->add_symbol(constraint->m_name, &constraint->base);
        auto *extra = ASR::down_cast<ASR::symbol_t>(ASR::make_TraitConstraint_t(
            al, constraint->base.base.loc, scope, LCompilers::s2c(al, "extra"),
            constraint->m_parameter, constraint->m_trait, nullptr, 0, nullptr, 0));
        scope->add_symbol("extra", extra);
        rejected("asr.verify.trait.inline_has_one_binder");
        scope->erase_symbol("extra");
        auto *member = ASR::down_cast<ASR::Integer_t>(trait->m_member_types[0]);
        member->m_kind = 1001;
        rejected("asr.verify.trait.numeric_member");
        member->m_kind = 4;
        CHECK(LCompilers::asr_verify(*unit, true, diagnostics));
    };
    check(result.result);
    LCompilers::SymbolTable symtab(nullptr);
    auto *copy = ASR::down_cast2<ASR::TranslationUnit_t>(LCompilers::deserialize_asr(
        al, LCompilers::serialize(*result.result), true, symtab, 0));
    fix_external_symbols(*copy, symtab);
    check(copy);
}

TEST_CASE("Inline numeric constraints stage unsupported syntax and capabilities") {
    const std::string prefix = R"(
module inline_stages
implicit none
abstract interface :: IValue
    function value() result(r)
        integer :: r
    end function
end interface
contains
function unused{)";
    const std::string suffix = R"( :: T}(x) result(r)
    type(T), intent(in) :: x
    type(T) :: r
    r = x
end function
end module
)";
    struct Case {
        std::string constraint;
        bool parsed;
        std::string message;
    };
    for (const auto &test : std::vector<Case>{
            {"integer(4) + IValue", false, "unexpected"},
            {"IValue | integer", false, "unexpected"},
            {"integer |", false, "unexpected"},
            {"IValue + integer", true, "type-set trait composition is not implemented yet"},
            {"integer(*)", true, "type-set kind wildcards are not implemented yet"},
            {"real(kind=*)", true, "type-set kind wildcards are not implemented yet"},
            {"logical(4)", true, "non-numeric type-set members are not implemented yet"},
            {"tuple(integer, real)", false, "unexpected"}}) {
        Allocator al(64 * 1024);
        LCompilers::CompilerOptions options;
        LCompilers::diag::Diagnostics diagnostics;
        LCompilers::LocationManager lm;
        auto source = prefix + test.constraint + suffix;
        auto parsed = LCompilers::LFortran::parse(al, source, diagnostics, options);
        INFO(test.constraint);
        REQUIRE(parsed.ok == test.parsed);
        if (test.parsed) {
            auto result = LCompilers::LFortran::ast_to_asr(
                al, *parsed.result, diagnostics, nullptr, false, options, lm);
            CHECK_FALSE(result.ok);
        }
        REQUIRE(!diagnostics.diagnostics.empty());
        const auto &error = diagnostics.diagnostics.back();
        CHECK(error.stage == (test.parsed ? LCompilers::diag::Stage::Semantic
                                         : LCompilers::diag::Stage::Parser));
        CHECK(error.message.find(test.message) != std::string::npos);
        REQUIRE(!error.labels.empty());
        CHECK(!error.labels[0].spans.empty());
    }
}

TEST_CASE("Numeric trait source, binary, text and module roundtrips") {
    namespace ASR = LCompilers::ASR;
    namespace ASRUtils = LCompilers::ASRUtils;
    ast_ser(numeric_trait_source);
    asr_ser(numeric_trait_source);
    asr_mod(numeric_trait_source, "numeric_contracts");

    Allocator al(1024 * 1024);
    LCompilers::CompilerOptions options;
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::LocationManager lm;
    auto parsed = LCompilers::LFortran::parse(al, numeric_trait_source, diagnostics, options);
    REQUIRE(parsed.ok);
    auto result = LCompilers::LFortran::ast_to_asr(
        al, *parsed.result, diagnostics, nullptr, false, options, lm);
    INFO(diagnostics.render2());
    REQUIRE(result.ok);
    auto *module = ASR::down_cast<ASR::Module_t>(
        result.result->m_symtab->get_symbol("numeric_contracts"));
    for (const auto &name : {"first", "second", "helper"}) {
        auto *constraint = numeric_constraint(module, name);
        REQUIRE(constraint);
        CHECK(constraint->n_intrinsic_requirements == 2);
        for (size_t i = 0; i < constraint->n_intrinsic_requirements; i++) {
            CHECK(constraint->m_intrinsic_requirements[i].n_witnesses == 2);
        }
    }
    CHECK(numeric_constraint(module, "unused_arithmetic")->n_intrinsic_requirements == 5);
    CHECK(numeric_constraint(module, "unused_compare")->n_intrinsic_requirements == 2);
    auto *program = ASR::down_cast<ASR::Program_t>(
        result.result->m_symtab->get_symbol("numeric_client"));
    auto callee = [&](size_t index) {
        auto *assignment = ASR::down_cast<ASR::Assignment_t>(program->m_body[index]);
        return ASRUtils::symbol_get_past_external(
            ASR::down_cast<ASR::FunctionCall_t>(assignment->m_value)->m_name);
    };
    CHECK(callee(0) == callee(1));
    CHECK(callee(2) == callee(3));
    CHECK(callee(0) != callee(2));
    struct ArrayItems : ASR::BaseWalkVisitor<ArrayItems> {
        std::vector<ASR::ttype_t*> types;
        void visit_ArrayItem(const ASR::ArrayItem_t &x) { types.push_back(x.m_type); }
    };
    for (size_t i : {4u, 5u}) {
        auto *sum = ASR::down_cast<ASR::Function_t>(callee(i));
        ArrayItems items;
        for (size_t j = 0; j < sum->n_body; j++) items.visit_stmt(*sum->m_body[j]);
        REQUIRE(items.types.size() == 1);
        CHECK(ASRUtils::types_equal(items.types[0],
            ASRUtils::expr_type(sum->m_return_var), nullptr, nullptr));
        CHECK(ASRUtils::extract_kind_from_ttype_t(items.types[0]) == (i == 4 ? 4 : 8));
    }
    size_t real_conversions = 0;
    for (const auto &entry : program->m_symtab->get_scope()) {
        if (!ASR::is_a<ASR::Function_t>(*entry.second)) continue;
        auto *fn = ASR::down_cast<ASR::Function_t>(entry.second);
        if (fn->n_body != 1 || !ASR::is_a<ASR::Assignment_t>(*fn->m_body[0])) continue;
        auto *value = ASR::down_cast<ASR::Assignment_t>(fn->m_body[0])->m_value;
        if (!ASR::is_a<ASR::IntrinsicElementalFunction_t>(*value) ||
                !ASR::is_a<ASR::Real_t>(*ASRUtils::expr_type(value))) continue;
        auto *conversion = ASR::down_cast<ASR::IntrinsicElementalFunction_t>(value);
        REQUIRE(conversion->n_args == 1);
        CHECK(ASR::is_a<ASR::Integer_t>(*ASRUtils::expr_type(conversion->m_args[0])));
        CHECK(ASRUtils::extract_kind_from_ttype_t(ASRUtils::expr_type(value)) == 8);
        real_conversions++;
    }
    CHECK(real_conversions > 0);
    auto *root_trait = ASRUtils::symbol_get_past_external(
        program->m_symtab->get_symbol("rootnumeric"));
    auto *other_trait = ASRUtils::symbol_get_past_external(
        program->m_symtab->get_symbol("othernumeric"));
    CHECK(root_trait != other_trait);
    CHECK(ASR::down_cast<ASR::Trait_t>(root_trait)->n_member_types == 2);
    CHECK(ASR::down_cast<ASR::Trait_t>(other_trait)->n_member_types == 1);

    auto check_roundtrip = [&](ASR::TranslationUnit_t *unit) {
        auto *client = ASR::down_cast<ASR::Program_t>(
            unit->m_symtab->get_symbol("numeric_client"));
        auto *one = ASR::down_cast<ASR::FunctionCall_t>(
            ASR::down_cast<ASR::Assignment_t>(client->m_body[0])->m_value);
        auto *two = ASR::down_cast<ASR::FunctionCall_t>(
            ASR::down_cast<ASR::Assignment_t>(client->m_body[1])->m_value);
        CHECK(ASRUtils::symbol_get_past_external(one->m_name) ==
            ASRUtils::symbol_get_past_external(two->m_name));
        CHECK(LCompilers::asr_verify(*unit, true, diagnostics));
    };
    for (auto form : {LCompilers::ASRTextForm::Named, LCompilers::ASRTextForm::Positional}) {
        LCompilers::ASRTextOptions text_options;
        text_options.form = form;
        auto text = LCompilers::asr_to_text(*result.result, text_options);
        Allocator text_al(1024 * 1024);
        LCompilers::LocationManager text_lm;
        auto loaded = LCompilers::asr_from_text(
            text_al, text, "numeric.asr", text_lm, diagnostics);
        REQUIRE(loaded.ok);
        check_roundtrip(loaded.result);
        CHECK(text == LCompilers::asr_to_text(*loaded.result, text_options));
    }
    auto printed = LCompilers::LFortran::ast_to_src(*parsed.result);
    auto reparsed = LCompilers::LFortran::parse(al, printed, diagnostics, options);
    REQUIRE(reparsed.ok);
    CHECK(printed.find("integer | real(rk)") != std::string::npos);
    CHECK(printed.find("complex(rk)") != std::string::npos);
    auto rebuilt = LCompilers::LFortran::ast_to_asr(
        al, *reparsed.result, diagnostics, nullptr, false, options, lm);
    REQUIRE(rebuilt.ok);
    check_roundtrip(rebuilt.result);
    CHECK(LCompilers::asr_to_text(*rebuilt.result) ==
        LCompilers::asr_to_text(*result.result));
}

TEST_CASE("Numeric trait proofs reject incomplete and forged witnesses") {
    namespace ASR = LCompilers::ASR;
    namespace ASRUtils = LCompilers::ASRUtils;
    Allocator al(1024 * 1024);
    LCompilers::CompilerOptions options;
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::LocationManager lm;
    auto parsed = LCompilers::LFortran::parse(al, numeric_trait_source, diagnostics, options);
    REQUIRE(parsed.ok);
    auto result = LCompilers::LFortran::ast_to_asr(
        al, *parsed.result, diagnostics, nullptr, false, options, lm);
    REQUIRE(result.ok);
    auto *module = ASR::down_cast<ASR::Module_t>(
        result.result->m_symtab->get_symbol("numeric_contracts"));
    auto *constraint = numeric_constraint(module, "helper");
    REQUIRE(constraint);
    auto *trait = ASR::down_cast<ASR::Trait_t>(
        ASRUtils::symbol_get_past_external(constraint->m_trait));
    auto rejected = [&](const std::string &code) {
        LCompilers::diag::Diagnostics errors;
        CHECK_FALSE(LCompilers::asr_verify(*result.result, true, errors));
        REQUIRE(!errors.diagnostics.empty());
        INFO(errors.render2());
        CHECK(errors.diagnostics.back().code == code);
        CHECK(!errors.diagnostics.back().labels.empty());
    };
    auto &proof = constraint->m_intrinsic_requirements[0];
    REQUIRE(proof.n_witnesses == 2);
    size_t requirement_count = constraint->n_intrinsic_requirements;
    constraint->n_intrinsic_requirements = 0;
    rejected("asr.verify.type_set.restriction_record");
    constraint->n_intrinsic_requirements = requirement_count;
    proof.n_witnesses--;
    rejected("asr.verify.type_set.total_proof");
    proof.n_witnesses++;
    auto *member = proof.m_witnesses[1].m_member_type;
    proof.m_witnesses[1].m_member_type = proof.m_witnesses[0].m_member_type;
    rejected("asr.verify.type_set.unique_witness");
    proof.m_witnesses[1].m_member_type = ASRUtils::TYPE(ASR::make_Real_t(al, proof.loc, 4));
    rejected("asr.verify.type_set.member_in_set");
    proof.m_witnesses[1].m_member_type = member;
    auto *witness = ASR::down_cast<ASR::Function_t>(proof.m_witnesses[1].m_procedure);
    auto *ret = ASRUtils::EXPR2VAR(witness->m_return_var);
    auto *ret_type = ret->m_type;
    ret->m_type = ASRUtils::TYPE(ASR::make_Real_t(al, proof.loc, 4));
    rejected("asr.verify.type_set.witness_type");
    ret->m_type = ret_type;
    auto *witness_signature = ASRUtils::get_FunctionType(witness);
    auto *signature_argument = witness_signature->m_arg_types[0];
    witness_signature->m_arg_types[0] = ASRUtils::TYPE(
        ASR::make_Integer_t(al, proof.loc, 1001));
    rejected("asr.verify.function.argument_type_matches_signature");
    witness_signature->m_arg_types[0] = signature_argument;
    auto *signature_result = witness_signature->m_return_var_type;
    witness_signature->m_return_var_type = ASRUtils::TYPE(
        ASR::make_Real_t(al, proof.loc, 1001));
    rejected("asr.verify.function.return_type_matches_signature");
    witness_signature->m_return_var_type = signature_result;
    auto *witness_argument = ASRUtils::EXPR2VAR(witness->m_args[0]);
    auto *argument_type = witness_argument->m_type;
    witness_argument->m_type = ASRUtils::TYPE(
        ASR::make_Integer_t(al, proof.loc, 1001));
    rejected("asr.verify.type_set.witness_locals");
    witness_argument->m_type = argument_type;
    auto *restriction_signature = ASRUtils::get_FunctionType(
        ASR::down_cast<ASR::Function_t>(proof.m_procedure));
    auto *restriction_argument = restriction_signature->m_arg_types[0];
    restriction_signature->m_arg_types[0] = ASRUtils::TYPE(
        ASR::make_Integer_t(al, proof.loc, 1001));
    rejected("asr.verify.function.argument_type_matches_signature");
    restriction_signature->m_arg_types[0] = restriction_argument;
    auto *operation = proof.m_operation;
    proof.m_operation = ASR::down_cast<ASR::type_set_operation_t>(
        ASR::make_TypeSetBinary_t(al, proof.loc, ASR::Sub));
    rejected("asr.verify.type_set.arity");
    proof.m_operation = operation;

    ASR::type_set_requirement_t *arithmetic = nullptr;
    for (size_t i = 0; i < constraint->n_intrinsic_requirements; i++) {
        if (ASR::is_a<ASR::TypeSetBinary_t>(*constraint->m_intrinsic_requirements[i].m_operation)) {
            arithmetic = &constraint->m_intrinsic_requirements[i];
        }
    }
    REQUIRE(arithmetic);
    auto *arithmetic_op = ASR::down_cast<ASR::TypeSetBinary_t>(arithmetic->m_operation);
    auto saved_op = arithmetic_op->m_op;
    arithmetic_op->m_op = ASR::Sub;
    rejected("asr.verify.type_set.witness_operation");
    arithmetic_op->m_op = saved_op;

    auto *integer_witness = ASR::down_cast<ASR::Function_t>(proof.m_witnesses[0].m_procedure);
    auto *assignment = ASR::down_cast<ASR::Assignment_t>(integer_witness->m_body[0]);
    auto *expression = assignment->m_value;
    assignment->m_value = integer_witness->m_args[0];
    CHECK(LCompilers::asr_verify(*result.result, true, diagnostics));
    assignment->m_value = ASRUtils::EXPR(ASR::make_Cast_t(al, proof.loc,
        integer_witness->m_args[0], ASR::cast_kindType::IntegerToInteger,
        ASRUtils::expr_type(integer_witness->m_return_var), nullptr, nullptr));
    CHECK(LCompilers::asr_verify(*result.result, true, diagnostics));
    assignment->m_value = ASRUtils::EXPR(ASR::make_IntegerBinOp_t(al, proof.loc,
        integer_witness->m_args[0], ASR::Add, integer_witness->m_args[0],
        ASRUtils::expr_type(integer_witness->m_return_var), nullptr));
    rejected("asr.verify.type_set.witness_operation");
    assignment->m_value = expression;

    auto &complex_proof = numeric_constraint(module, "unused_equal")->m_intrinsic_requirements[0];
    auto *comparison = ASR::down_cast<ASR::TypeSetComparison_t>(complex_proof.m_operation);
    auto *complex_fn = ASR::down_cast<ASR::Function_t>(complex_proof.m_witnesses[0].m_procedure);
    auto *compare_expr = ASR::down_cast<ASR::ComplexCompare_t>(
        ASR::down_cast<ASR::Assignment_t>(complex_fn->m_body[0])->m_value);
    comparison->m_op = ASR::Lt;
    compare_expr->m_op = ASR::Lt;
    rejected("asr.verify.type_set.witness_operation");
    comparison->m_op = ASR::Eq;
    compare_expr->m_op = ASR::Eq;

    trait->m_kind = ASR::trait_kindType::UniversalTrait;
    rejected("asr.verify.trait_constraint.intrinsic_category");
    trait->m_kind = ASR::trait_kindType::IntrinsicTypeSet;
    auto *box = module->m_symtab->get_symbol("box");
    auto *bad_impl = ASR::down_cast<ASR::symbol_t>(ASR::make_TraitImplementation_t(
        al, proof.loc, module->m_symtab, LCompilers::s2c(al, "bad_impl"),
        ASRUtils::make_StructType_t_util(al, proof.loc, box, true), box, &trait->base,
        nullptr, 0, ASR::accessType::Private));
    module->m_symtab->add_symbol("bad_impl", bad_impl);
    rejected("asr.verify.trait_implementation.not_type_set");
    module->m_symtab->erase_symbol("bad_impl");

    auto *program = ASR::down_cast<ASR::Program_t>(
        result.result->m_symtab->get_symbol("numeric_client"));
    auto *call = ASR::down_cast<ASR::FunctionCall_t>(
        ASR::down_cast<ASR::Assignment_t>(program->m_body[0])->m_value);
    auto *callee = call->m_name;
    call->m_name = proof.m_procedure;
    rejected("asr.verify.call.unbound_restriction");
    call->m_name = callee;
    auto *function = ASR::down_cast<ASR::Function_t>(ASRUtils::symbol_get_past_external(callee));
    auto *concrete_arg = ASRUtils::EXPR2VAR(function->m_args[0]);
    auto *concrete_type = concrete_arg->m_type;
    concrete_arg->m_type = ASRUtils::symbol_type(constraint->m_parameter);
    rejected("asr.verify.type_parameter.concrete_executable");
    concrete_arg->m_type = concrete_type;
    auto *main_variable = ASR::down_cast<ASR::Variable_t>(
        program->m_symtab->get_symbol("i"));
    auto *main_type = main_variable->m_type;
    main_variable->m_type = ASRUtils::symbol_type(constraint->m_parameter);
    rejected("asr.verify.type_parameter.concrete_executable");
    main_variable->m_type = main_type;
    CHECK(LCompilers::asr_verify(*result.result, true, diagnostics));

    // A concrete legacy operator witness can retain is_restriction, including
    // in an interface loaded from a separately compiled module.
    auto *signature = ASRUtils::get_FunctionType(function);
    auto definition = signature->m_deftype;
    bool restriction = signature->m_is_restriction;
    size_t body_size = function->n_body, dependencies = function->n_dependencies;
    signature->m_deftype = ASR::deftypeType::Interface;
    signature->m_is_restriction = true;
    function->n_body = 0;
    function->n_dependencies = 0;
    CHECK(LCompilers::asr_verify(*result.result, true, diagnostics));
    signature->m_deftype = definition;
    signature->m_is_restriction = restriction;
    function->n_body = body_size;
    function->n_dependencies = dependencies;
}

TEST_CASE("Numeric trait finite proofs require concrete intrinsic kinds") {
    namespace ASR = LCompilers::ASR;
    struct DeferredKind : ASR::BaseWalkVisitor<DeferredKind> {
        ASR::ttypeType category;
        int64_t kind;
        size_t changed = 0;
        DeferredKind(ASR::ttypeType category, int64_t kind)
            : category(category), kind(kind) {}
        void visit_Integer(const ASR::Integer_t &x) {
            if (category == ASR::ttypeType::Integer) {
                const_cast<ASR::Integer_t&>(x).m_kind = kind;
                changed++;
            }
        }
        void visit_Real(const ASR::Real_t &x) {
            if (category == ASR::ttypeType::Real) {
                const_cast<ASR::Real_t&>(x).m_kind = kind;
                changed++;
            }
        }
        void visit_Complex(const ASR::Complex_t &x) {
            if (category == ASR::ttypeType::Complex) {
                const_cast<ASR::Complex_t&>(x).m_kind = kind;
                changed++;
            }
        }
        void visit_Logical(const ASR::Logical_t &x) {
            if (category == ASR::ttypeType::Logical) {
                const_cast<ASR::Logical_t&>(x).m_kind = kind;
                changed++;
            }
        }
    };
    auto check_mutation = [&](const std::string &source, ASR::ttypeType category,
            const std::string &code) {
        Allocator al(1024 * 1024);
        LCompilers::CompilerOptions options;
        LCompilers::diag::Diagnostics diagnostics;
        LCompilers::LocationManager lm;
        auto parsed = LCompilers::LFortran::parse(al, source, diagnostics, options);
        REQUIRE(parsed.ok);
        auto result = LCompilers::LFortran::ast_to_asr(
            al, *parsed.result, diagnostics, nullptr, false, options, lm);
        INFO(diagnostics.render2());
        REQUIRE(result.ok);
        REQUIRE(LCompilers::asr_verify(*result.result, true, diagnostics));
        auto binary = LCompilers::serialize(*result.result);
        Allocator loaded_al(1024 * 1024);
        LCompilers::SymbolTable scope(nullptr);
        auto *loaded = ASR::down_cast2<ASR::TranslationUnit_t>(
            LCompilers::deserialize_asr(loaded_al, binary, true, scope, 0));
        fix_external_symbols(*loaded, scope);
        REQUIRE(LCompilers::asr_verify(*loaded, true, diagnostics));
        auto rejected = [&](ASR::TranslationUnit_t *unit) {
            LCompilers::diag::Diagnostics errors;
            CHECK_FALSE(LCompilers::asr_verify(*unit, true, errors));
            REQUIRE(!errors.diagnostics.empty());
            INFO(errors.render2());
            CHECK(errors.diagnostics.back().code == code);
            CHECK(!errors.diagnostics.back().labels.empty());
        };
        for (auto *unit : {result.result, loaded}) {
            for (int64_t kind : {1000, 1001}) {
                CAPTURE(kind);
                DeferredKind mutation(category, kind);
                mutation.visit_TranslationUnit(*unit);
                REQUIRE(mutation.changed > 0);
                rejected(unit);
            }
        }
    };
    struct Member {
        const char *source;
        ASR::ttypeType category;
    };
    for (const auto &member : {
            Member{"integer", ASR::ttypeType::Integer},
            Member{"real(8)", ASR::ttypeType::Real},
            Member{"complex(8)", ASR::ttypeType::Complex}}) {
        for (const auto &expression : {"x", "x + x"}) {
            CAPTURE(member.source);
            CAPTURE(expression);
            auto source = std::string(R"(
module numeric_concrete_kind
implicit none
abstract interface :: a_numeric
    )") + member.source + R"(
end interface
contains
function unused{a_numeric :: T}(x) result(r)
    type(T), intent(in) :: x
    type(T) :: r
    r = )" + expression + R"(
end function
end module
)";
            check_mutation(source, member.category, "asr.verify.trait.numeric_member");
        }
    }
    check_mutation(R"(
module numeric_source_kind
implicit none
abstract interface :: a_numeric
    real(8)
end interface
contains
function unused{a_numeric :: T}(n) result(r)
    integer, intent(in) :: n
    type(T) :: r
    r = T(n)
end function
end module
)", ASR::ttypeType::Integer, "asr.verify.type_set.signature");
    check_mutation(R"(
module numeric_result_kind
implicit none
abstract interface :: a_numeric
    integer
end interface
contains
function unused{a_numeric :: T}(x) result(r)
    type(T), intent(in) :: x
    logical :: r
    r = x < x
end function
end module
)", ASR::ttypeType::Logical, "asr.verify.type_set.signature");
    const std::string pdt = R"(
module numeric_pdt_control
implicit none
type :: box(k)
    integer, kind :: k
    integer(k) :: value
end type
end module
)";
    asr_ser(pdt);
    asr_mod(pdt, "numeric_pdt_control");
}

TEST_CASE("Numeric trait intrinsic boundaries and ordinary applicability") {
    const std::string prefix = R"(
module numeric_intrinsic_boundary
implicit none
abstract interface :: INumeric
    integer | real(8)
end interface
contains
function unused{INumeric :: T}(x) result(r)
    type(T), intent(in) :: x
    type(T) :: r
    r = )";
    for (const auto &expr : {"abs(x)", "abs(a=x)", "mod(x,x)", "sqrt(x)", "real(x)",
                            "int(x)", "cmplx(x)", "kind(x)", "dble(x)", "dble(a=x)",
                            "reshape([x],[1])", "-x", "x**x"}) {
        CAPTURE(expr);
        Allocator al(1024 * 1024);
        LCompilers::CompilerOptions options;
        LCompilers::diag::Diagnostics diagnostics;
        LCompilers::LocationManager lm;
        auto source = prefix + expr + "\nend function\nend module\n";
        auto parsed = LCompilers::LFortran::parse(al, source, diagnostics, options);
        REQUIRE(parsed.ok);
        auto result = LCompilers::LFortran::ast_to_asr(
            al, *parsed.result, diagnostics, nullptr, false, options, lm);
        CHECK_FALSE(result.ok);
        REQUIRE(diagnostics.has_error());
        CHECK(diagnostics.diagnostics.back().message.find("not implemented yet") != std::string::npos);
    }
    for (const auto &source : {
            "module m\ncontains\nlogical function less(x,y)\ncomplex(8) :: x,y\n"
                "less=x<y\nend function\nend module\n",
            "module m\ncontains\ncomplex(8) function convert(x)\nlogical :: x\n"
                "convert=cmplx(x,kind=8)\nend function\nend module\n"}) {
        Allocator al(1024 * 1024);
        LCompilers::CompilerOptions options;
        LCompilers::diag::Diagnostics diagnostics;
        LCompilers::LocationManager lm;
        auto parsed = LCompilers::LFortran::parse(al, source, diagnostics, options);
        REQUIRE(parsed.ok);
        auto result = LCompilers::LFortran::ast_to_asr(
            al, *parsed.result, diagnostics, nullptr, false, options, lm);
        CHECK_FALSE(result.ok);
        CHECK(diagnostics.has_error());
    }
}

TEST_CASE("Numeric trait constructors respect nested value shadowing") {
    const std::string source = R"(
module numeric_shadow
implicit none
abstract interface :: INumeric
    integer | real(8)
end interface
contains
function identity{INumeric :: T}(x) result(r)
    type(T), intent(in) :: x
    type(T) :: r
    r = x
contains
integer function nested(T) result(r)
    integer, intent(in) :: T(:)
    r = T(1)
end function
end function
end module
)";
    ast_ser(source);
    asr_ser(source);
    asr_mod(source);
}

TEST_CASE("Numeric trait staged forms do not create unchecked specializations") {
    const std::vector<std::pair<std::string, std::string>> sources = {
        {R"(
module m
implicit none
abstract interface :: INumeric
    integer | real(8)
end interface
contains
function unused{INumeric :: T}(x) result(r)
    type(T), intent(in) :: x(:)
    type(T) :: r(size(x))
    r = reshape(x, [size(x)])
end function
end module
)", "intrinsic 'reshape' on a type-set parameter is not implemented yet"},
        {R"(
module m
implicit none
abstract interface :: INumeric
    integer | real(8)
end interface
contains
pure function extent{INumeric :: T}(x) result(r)
    type(T), intent(in) :: x
    integer :: r
    r = 1
end function
subroutine use_extent(x, y)
    integer, intent(in) :: x, y(extent{integer}(x))
end subroutine
end module
)", "type-set generic calls in specification expressions are not implemented yet"},
        {R"(
module m
implicit none
abstract interface :: IComplex
    complex(8)
end interface
interface operator(<)
    module procedure compare
end interface
contains
logical function compare(x, y)
    complex(8), intent(in) :: x, y
    compare = .false.
end function
function unused{IComplex :: T}(x, y) result(r)
    type(T), intent(in) :: x, y
    logical :: r
    r = x < y
end function
end module
)", "operator '<' is not available for type-set member complex(8)"}
    };
    for (const auto &source : sources) {
        CAPTURE(source.second);
        Allocator al(1024 * 1024);
        LCompilers::CompilerOptions options;
        LCompilers::diag::Diagnostics diagnostics;
        LCompilers::LocationManager lm;
        auto parsed = LCompilers::LFortran::parse(al, source.first, diagnostics, options);
        REQUIRE(parsed.ok);
        auto result = LCompilers::LFortran::ast_to_asr(
            al, *parsed.result, diagnostics, nullptr, false, options, lm);
        CHECK_FALSE(result.ok);
        REQUIRE(diagnostics.has_error());
        INFO(diagnostics.render2());
        CHECK(diagnostics.diagnostics.back().message == source.second);
    }
}

static const std::string trait_serialization_source = R"(
module trait_serialization_m
implicit none
abstract interface :: IValue
    function get_value() result(value)
        integer :: value
    end function
end interface
abstract interface :: IOther
    function get_value() result(value)
        integer :: value
    end function
end interface
type :: Box
    integer :: value
end type
implements IValue :: Box
    procedure, pass :: get_value => box_value
end implements
contains
function box_value(self) result(value)
    class(Box), intent(in) :: self
    integer :: value
    value = self%value
end function
function read_value{IValue :: T}(object) result(value)
    type(T), intent(in) :: object
    integer :: value
    value = object%get_value()
end function
end module
)";

TEST_CASE("Trait AST and ASR serialization") {
    ast_ser(trait_serialization_source);
    ast_ser("program p\ninteger :: implements\nimplements = 1\nend program");
    asr_ser(trait_serialization_source);
    asr_mod(trait_serialization_source);
}

static const std::string runtime_trait_serialization_source = R"(
module runtime_trait_serialization_m
implicit none
abstract interface :: IValue
    pure function value() result(r)
        integer :: r
    end function
    function tag(code) result(r)
        integer, intent(in) :: code
        integer :: r
    end function
end interface
type :: Payload
    integer :: n
end type
type :: Other
    integer :: n
end type
implements IValue :: Payload
    procedure, pass :: value => read_value
    procedure, nopass :: tag => read_tag
end implements
contains
pure function read_value(self) result(r)
    class(Payload), intent(in) :: self
    integer :: r
    r = self%n
end function
function read_tag(code) result(r)
    integer, intent(in) :: code
    integer :: r
    r = 100 + code
end function
function observe(object) result(r)
    class(IValue), intent(in) :: object
    integer :: r
    r = object%value()
end function
function forward(object) result(r)
    class(IValue), intent(in) :: object
    integer :: r
    r = observe(object)
end function
function construct(object) result(r)
    type(Payload), intent(in) :: object
    integer :: r
    r = forward(object)
end function
end module
)";

TEST_CASE("Runtime trait ASR round trips and checked borrowing") {
    namespace ASR = LCompilers::ASR;
    namespace ASRUtils = LCompilers::ASRUtils;
    ast_ser(runtime_trait_serialization_source);
    asr_ser(runtime_trait_serialization_source);
    asr_mod(runtime_trait_serialization_source);
    Allocator al(1024 * 1024);
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::CompilerOptions options;
    auto parsed = LCompilers::LFortran::parse(al, runtime_trait_serialization_source,
        diagnostics, options);
    REQUIRE(parsed.ok);
    LCompilers::LocationManager lm;
    auto result = LCompilers::LFortran::ast_to_asr(
        al, *parsed.result, diagnostics, nullptr, false, options, lm);
    INFO(diagnostics.render2());
    REQUIRE(result.ok);
    auto *module = ASR::down_cast<ASR::Module_t>(
        result.result->m_symtab->get_symbol("runtime_trait_serialization_m"));
    auto *contract = ASRUtils::trait_runtime_contract(module->m_symtab->get_symbol("ivalue"));
    REQUIRE(contract);
    REQUIRE(contract->n_slots == 2);
    ASR::TraitWitness_t *witness = nullptr;
    for (auto &entry : module->m_symtab->get_scope()) {
        if (ASR::is_a<ASR::TraitWitness_t>(*entry.second)) {
            witness = ASR::down_cast<ASR::TraitWitness_t>(entry.second);
        }
    }
    REQUIRE(witness);
    auto *implementation = ASR::down_cast<ASR::TraitImplementation_t>(
        ASRUtils::symbol_get_past_external(witness->m_implementation));
    auto function = [&](const std::string &name) {
        return ASR::down_cast<ASR::Function_t>(module->m_symtab->get_symbol(name));
    };
    auto *observe = function("observe");
    auto *dispatch = ASR::down_cast<ASR::TraitFunctionCall_t>(
        ASR::down_cast<ASR::Assignment_t>(observe->m_body[0])->m_value);
    auto *construction = ASR::down_cast<ASR::FunctionCall_t>(
        ASR::down_cast<ASR::Assignment_t>(function("construct")->m_body[0])->m_value);
    auto *pack = ASR::down_cast<ASR::TraitPack_t>(construction->m_args[0].m_value);
    auto *forward = ASR::down_cast<ASR::FunctionCall_t>(
        ASR::down_cast<ASR::Assignment_t>(function("forward")->m_body[0])->m_value);
    CHECK(ASR::is_a<ASR::Var_t>(*forward->m_args[0].m_value));
    CHECK(ASRUtils::symbol_get_past_external(pack->m_witness) == &witness->base);
    CHECK(ASRUtils::symbol_get_past_external(implementation->m_type_declaration) ==
        module->m_symtab->get_symbol("payload"));
    CHECK(ASR::down_cast<ASR::Struct_t>(module->m_symtab->get_symbol("payload"))->m_parent == nullptr);

    auto rejects = [&](const std::string &code) {
        LCompilers::diag::Diagnostics invalid;
        CHECK_FALSE(LCompilers::asr_verify(*result.result, true, invalid));
        INFO(invalid.render2());
        REQUIRE(!invalid.diagnostics.empty());
        CHECK(invalid.diagnostics.back().code == code);
    };
    SUBCASE("text named and positional preserve canonical witness references") {
        for (auto form : {LCompilers::ASRTextForm::Named, LCompilers::ASRTextForm::Positional}) {
            LCompilers::ASRTextOptions text_options;
            text_options.form = form;
            std::string text = LCompilers::asr_to_text(*result.result, text_options);
            LCompilers::diag::Diagnostics loaded_diagnostics;
            LCompilers::LocationManager loaded_lm;
            auto loaded = LCompilers::asr_from_text(al, text, "runtime_traits.asr",
                loaded_lm, loaded_diagnostics);
            INFO(loaded_diagnostics.render2());
            REQUIRE(loaded.ok);
            CHECK(LCompilers::asr_verify(*loaded.result, true, loaded_diagnostics));
            CHECK(text == LCompilers::asr_to_text(*loaded.result, text_options));
        }
    }
    SUBCASE("missing witness slot") {
        witness->n_procedures = 0;
        rejects("asr.verify.trait_witness.complete");
    }
    SUBCASE("a contract slot cannot name a trait instead of a procedure") {
        contract->m_slots[0].m_procedure = contract->m_trait;
        rejects("asr.verify.trait_contract.procedure");
    }
    SUBCASE("a referenced slot must have its function signature") {
        auto *slot = ASR::down_cast<ASR::Function_t>(contract->m_slots[0].m_procedure);
        slot->m_function_signature = nullptr;
        rejects("asr.verify.trait_procedure.signature");
    }
    SUBCASE("a referenced contract must have its slot records") {
        contract->m_slots = nullptr;
        rejects("asr.verify.trait_contract.complete");
    }
    SUBCASE("missing selected witness") {
        pack->m_witness = nullptr;
        rejects("asr.verify.trait_pack.required_fields");
    }
    SUBCASE("wrong witness slot signature") {
        witness->m_procedures[0] = witness->m_procedures[1];
        witness->m_dependencies[0] = ASRUtils::symbol_name(witness->m_procedures[1]);
        rejects("asr.verify.trait_witness.signature");
    }
    SUBCASE("an adapter must belong to its witness scope") {
        witness->m_procedures[0] = &function("read_tag")->base;
        witness->m_dependencies[0] = function("read_tag")->m_name;
        rejects("asr.verify.trait_witness.unique_procedure");
    }
    SUBCASE("out of scope selected witness") {
        auto *reference = ASR::down_cast<ASR::symbol_t>(ASR::make_ExternalSymbol_t(
            al, pack->base.base.loc, observe->m_symtab, LCompilers::s2c(al, "foreign_witness"),
            &witness->base, module->m_name, nullptr, 0, witness->m_name,
            ASR::accessType::Private));
        observe->m_symtab->add_symbol("foreign_witness", reference);
        pack->m_witness = reference;
        rejects("asr.verify.trait_borrow.witness_in_scope");
    }
    SUBCASE("type set cannot supply a runtime view") {
        ASR::down_cast<ASR::Trait_t>(
            module->m_symtab->get_symbol("ivalue"))->m_kind =
                ASR::trait_kindType::IntrinsicTypeSet;
        rejects("asr.verify.trait_implementation.not_type_set");
    }
    SUBCASE("duplicate witness slot") {
        witness->m_procedures[1] = witness->m_procedures[0];
        rejects("asr.verify.trait_witness.unique_procedure");
    }
    SUBCASE("wrong dynamic slot") {
        dispatch->m_slot = 1 - dispatch->m_slot;
        rejects("asr.verify.trait_call.slot");
    }
    SUBCASE("wrong nominal pack payload") {
        ASRUtils::EXPR2VAR(pack->m_payload)->m_type_declaration =
            module->m_symtab->get_symbol("other");
        rejects("asr.verify.trait_pack.nominal_type");
    }
    SUBCASE("borrowed dummy is not an owner") {
        auto *var = ASRUtils::EXPR2VAR(observe->m_args[0]);
        var->m_type = ASRUtils::TYPE(ASR::make_Allocatable_t(al, var->base.base.loc, var->m_type));
        rejects("asr.verify.trait_owner.argument");
    }
    SUBCASE("borrowed view is not a plain local") {
        ASRUtils::EXPR2VAR(observe->m_args[0])->m_intent = ASR::intentType::Local;
        rejects("asr.verify.trait_view.borrowed_storage");
    }
    SUBCASE("binding has wrong result signature") {
        auto *proc = function("read_value");
        auto *type = ASRUtils::TYPE(ASR::make_Real_t(al, proc->base.base.loc, 4));
        ASRUtils::EXPR2VAR(proc->m_return_var)->m_type = type;
        ASRUtils::get_FunctionType(proc)->m_return_var_type = type;
        rejects("asr.verify.trait_witness.binding_signature");
    }
    SUBCASE("binding cannot drop required purity") {
        ASRUtils::get_FunctionType(function("read_value"))->m_pure = false;
        function("read_value")->m_side_effect_free = false;
        rejects("asr.verify.trait_witness.procedure_attributes");
    }
    SUBCASE("binding cannot drop required elemental attribute") {
        auto *required = ASR::down_cast<ASR::Function_t>(
            ASRUtils::symbol_get_past_external(implementation->m_bindings[0].m_member));
        ASRUtils::get_FunctionType(required)->m_elemental = true;
        rejects("asr.verify.trait_witness.procedure_attributes");
    }
    SUBCASE("binding can strengthen an impure contract to pure") {
        ASRUtils::get_FunctionType(function("read_tag"))->m_pure = true;
        function("read_tag")->m_side_effect_free = true;
        LCompilers::diag::Diagnostics valid;
        CHECK(LCompilers::asr_verify(*result.result, true, valid));
        INFO(valid.render2());
    }
    SUBCASE("binding cannot use an unsupported runtime ABI") {
        ASRUtils::get_FunctionType(function("read_value"))->m_abi = ASR::abiType::BindC;
        rejects("asr.verify.trait_witness.binding_abi");
    }
    SUBCASE("binding has wrong nominal receiver") {
        ASRUtils::EXPR2VAR(function("read_value")->m_args[0])->m_type_declaration =
            module->m_symtab->get_symbol("other");
        rejects("asr.verify.trait_witness.receiver_type");
    }
    SUBCASE("nominal origins cannot disappear") {
        contract->m_slots[0].n_origins = 0;
        rejects("asr.verify.trait_contract.origins");
    }
    SUBCASE("runtime interface cannot become an ordinary direct call") {
        ASR::down_cast<ASR::Assignment_t>(observe->m_body[0])->m_value =
            ASRUtils::EXPR(ASR::make_FunctionCall_t(al, dispatch->base.base.loc,
                dispatch->m_name, nullptr, dispatch->m_args, dispatch->n_args,
                dispatch->m_type, nullptr, nullptr));
        rejects("asr.verify.trait_call.dynamic_required");
    }
    SUBCASE("consumer cannot recover an arbitrary concrete payload") {
        const auto &loc = dispatch->base.base.loc;
        auto *recovery = ASRUtils::EXPR(ASR::make_TraitReceiver_t(al, loc,
            observe->m_args[0], &witness->base, implementation->m_type_declaration,
            implementation->m_implementing_type));
        ASR::call_arg_t *arg = al.allocate<ASR::call_arg_t>(1);
        arg->loc = loc;
        arg->m_value = recovery;
        ASR::down_cast<ASR::Assignment_t>(observe->m_body[0])->m_value =
            ASRUtils::EXPR(ASR::make_FunctionCall_t(al, loc,
                &function("read_value")->base, nullptr, arg, 1,
                dispatch->m_type, nullptr, nullptr));
        LCompilers::PassUtils::UpdateDependenciesVisitor dependencies(al);
        dependencies.visit_TranslationUnit(*result.result);
        rejects("asr.verify.trait_receiver.authorized_adapter");
    }
}

TEST_CASE("Runtime trait ownership round trips and storage proofs") {
    namespace ASR = LCompilers::ASR;
    namespace ASRUtils = LCompilers::ASRUtils;
    const std::string source = R"(
module owned_trait_serialization_m
abstract interface :: IValue
    function value() result(r)
        integer :: r
    end function
end interface
abstract interface :: IOther
    function other() result(r)
        integer :: r
    end function
end interface
type :: Payload
    integer :: n = 5
end type
type :: Other
    integer :: n = 6
end type
implements IValue :: Payload
    procedure, pass :: value => read_payload
end implements
class(IValue), allocatable :: stored
contains
function read_payload(self) result(r)
    class(Payload), intent(in) :: self
    integer :: r
    r = self%n
end function
subroutine exercise(view, other_view)
    class(IValue), intent(in) :: view
    class(IOther), intent(in) :: other_view
    type(Payload) :: data
    class(IValue), allocatable :: owner, copy
    class(*), allocatable :: erased
    allocate(Payload :: owner)
    deallocate(owner)
    allocate(owner, source=data)
    copy = owner
    stored = view
    deallocate(owner, copy)
end subroutine
end module
)";
    ast_ser(source);
    asr_ser(source);
    asr_mod(source);
    Allocator al(1024 * 1024);
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::CompilerOptions options;
    auto parsed = LCompilers::LFortran::parse(al, source, diagnostics, options);
    REQUIRE(parsed.ok);
    LCompilers::LocationManager lm;
    auto result = LCompilers::LFortran::ast_to_asr(
        al, *parsed.result, diagnostics, nullptr, false, options, lm);
    INFO(diagnostics.render2());
    REQUIRE(result.ok);
    auto *module = ASR::down_cast<ASR::Module_t>(
        result.result->m_symtab->get_symbol("owned_trait_serialization_m"));
    auto *function = ASR::down_cast<ASR::Function_t>(
        module->m_symtab->get_symbol("exercise"));
    auto *typed = ASR::down_cast<ASR::TraitAllocate_t>(function->m_body[0]);
    auto *sourced = ASR::down_cast<ASR::TraitAllocate_t>(function->m_body[2]);
    auto *copy = ASR::down_cast<ASR::TraitAssignment_t>(function->m_body[3]);
    auto *from_view = ASR::down_cast<ASR::TraitAssignment_t>(function->m_body[4]);
    auto *deallocate = ASR::down_cast<ASR::ExplicitDeallocate_t>(function->m_body[5]);
    auto *witness = ASR::down_cast<ASR::TraitWitness_t>(
        ASRUtils::symbol_get_past_external(typed->m_witness));
    auto *borrow = ASR::down_cast<ASR::TraitBorrow_t>(copy->m_value);
    auto *owner = ASRUtils::EXPR2VAR(typed->m_target);
    CHECK_FALSE(typed->m_copy_value);
    CHECK(sourced->m_copy_value);
    CHECK(sourced->m_witness == typed->m_witness);
    CHECK(copy->m_witness == nullptr);
    CHECK(ASRUtils::is_trait_owner(owner->m_type));
    CHECK(ASRUtils::symbol_get_past_external(witness->m_lifecycle.m_type_declaration) ==
        module->m_symtab->get_symbol("payload"));
    auto rejects = [&](const std::string &code) {
        LCompilers::diag::Diagnostics invalid;
        CHECK_FALSE(LCompilers::asr_verify(*result.result, true, invalid));
        INFO(invalid.render2());
        REQUIRE(!invalid.diagnostics.empty());
        CHECK(invalid.diagnostics.back().code == code);
    };
    SUBCASE("named and positional text retain ownership and lifecycle") {
        for (auto form : {LCompilers::ASRTextForm::Named, LCompilers::ASRTextForm::Positional}) {
            LCompilers::ASRTextOptions text_options;
            text_options.form = form;
            auto text = LCompilers::asr_to_text(*result.result, text_options);
            LCompilers::diag::Diagnostics loaded_diagnostics;
            LCompilers::LocationManager loaded_lm;
            auto loaded = LCompilers::asr_from_text(al, text, "owned_traits.asr",
                loaded_lm, loaded_diagnostics);
            INFO(loaded_diagnostics.render2());
            REQUIRE(loaded.ok);
            CHECK(LCompilers::asr_verify(*loaded.result, true, loaded_diagnostics));
            CHECK(text == LCompilers::asr_to_text(*loaded.result, text_options));
        }
    }
    SUBCASE("missing concrete lifecycle") {
        witness->m_lifecycle.m_type_declaration = nullptr;
        rejects("asr.verify.trait_witness.evidence_in_scope");
    }
    SUBCASE("layout-equal nominal lifecycle is not interchangeable") {
        witness->m_lifecycle.m_type_declaration = module->m_symtab->get_symbol("other");
        rejects("asr.verify.trait_witness.lifecycle");
    }
    SUBCASE("lifecycle must name a concrete type") {
        witness->m_lifecycle.m_type_declaration = module->m_symtab->get_symbol("ivalue");
        rejects("asr.verify.trait_witness.lifecycle");
    }
    SUBCASE("allocation requires a concrete witness or a source") {
        typed->m_witness = nullptr;
        rejects("asr.verify.trait_allocate.initialization");
    }
    SUBCASE("a typed allocation is not sourced initialization") {
        typed->m_copy_value = true;
        rejects("asr.verify.trait_allocate.initialization");
    }
    SUBCASE("typed allocation must agree with its selected witness") {
        typed->m_type_declaration = module->m_symtab->get_symbol("other");
        rejects("asr.verify.trait_allocate.nominal_type");
    }
    SUBCASE("abstract metadata cannot be used to construct an owner") {
        ASR::down_cast<ASR::Struct_t>(
            module->m_symtab->get_symbol("payload"))->m_is_abstract = true;
        function->m_symtab->erase_symbol("data");
        function->n_body = 1;
        rejects("asr.verify.trait_owner.concrete_type");
    }
    SUBCASE("a concrete source needs selected conformance") {
        sourced->m_witness = nullptr;
        rejects("asr.verify.trait_owner.source");
    }
    SUBCASE("a selected witness cannot be an arbitrary symbol") {
        sourced->m_witness = module->m_symtab->get_symbol("payload");
        rejects("asr.verify.trait_borrow.witness");
    }
    SUBCASE("layout-equal source declarations do not supply nominal conformance") {
        ASRUtils::EXPR2VAR(sourced->m_source)->m_type_declaration =
            module->m_symtab->get_symbol("other");
        rejects("asr.verify.trait_owner.nominal_type");
    }
    SUBCASE("ordinary assignment cannot duplicate ownership") {
        function->m_body[3] = ASRUtils::STMT(ASR::make_Assignment_t(al,
            copy->base.base.loc, copy->m_target, borrow->m_owner, nullptr, false, false));
        rejects("asr.verify.trait_owner.value_copy");
    }
    SUBCASE("association cannot duplicate ownership") {
        function->m_body[3] = ASRUtils::STMT(ASR::make_Associate_t(al,
            copy->base.base.loc, copy->m_target, borrow->m_owner));
        rejects("asr.verify.trait_owner.explicit_protocol");
    }
    SUBCASE("ordinary allocation cannot omit lifecycle evidence") {
        ASR::alloc_arg_t arg{};
        arg.loc = typed->base.base.loc;
        arg.m_a = typed->m_target;
        function->m_body[0] = ASRUtils::STMT(ASR::make_Allocate_t(al,
            arg.loc, &arg, 1, nullptr, nullptr, nullptr));
        rejects("asr.verify.trait_owner.explicit_protocol");
    }
    SUBCASE("ordinary allocation cannot source an owner or borrowed trait") {
        for (auto* source : {typed->m_target, function->m_args[0]}) {
            ASR::alloc_arg_t arg{};
            arg.loc = typed->base.base.loc;
            arg.m_a = ASRUtils::EXPR(ASR::make_Var_t(al, arg.loc,
                function->m_symtab->get_symbol("erased")));
            function->m_body[0] = ASRUtils::STMT(ASR::make_Allocate_t(al,
                arg.loc, &arg, 1, nullptr, nullptr, source));
            rejects("asr.verify.trait_owner.explicit_protocol");
        }
    }
    SUBCASE("ordinary allocation cannot use an unconverted trait mold type") {
        ASR::alloc_arg_t arg{};
        arg.loc = typed->base.base.loc;
        arg.m_a = ASRUtils::EXPR(ASR::make_Var_t(al, arg.loc,
            function->m_symtab->get_symbol("erased")));
        arg.m_type = ASRUtils::expr_type(function->m_args[0]);
        function->m_body[0] = ASRUtils::STMT(ASR::make_Allocate_t(al,
            arg.loc, &arg, 1, nullptr, nullptr, nullptr));
        rejects("asr.verify.trait_owner.explicit_protocol");
    }
    SUBCASE("nullification cannot discard an owned allocation") {
        auto *target = typed->m_target;
        function->m_body[0] = ASRUtils::STMT(ASR::make_Nullify_t(al,
            typed->base.base.loc, &target, 1));
        rejects("asr.verify.trait_owner.explicit_protocol");
    }
    SUBCASE("a borrowed view cannot be an assignment target") {
        copy->m_target = function->m_args[0];
        rejects("asr.verify.trait_owner.storage");
    }
    SUBCASE("cleanup cannot destroy borrowed storage") {
        deallocate->m_vars[0] = function->m_args[0];
        rejects("asr.verify.trait_owner.storage");
    }
    SUBCASE("one cleanup cannot destroy an owner twice") {
        deallocate->m_vars[1] = deallocate->m_vars[0];
        rejects("asr.verify.trait_owner.duplicate_cleanup");
    }
    SUBCASE("executable blocks retain ownership side effects") {
        auto* implicit = ASRUtils::STMT(ASR::make_ImplicitDeallocate_t(al,
            deallocate->base.base.loc, deallocate->m_vars, deallocate->n_vars));
        for (ASR::stmt_t* statement : {&typed->base, &copy->base,
                &deallocate->base, implicit}) {
            auto* block = ASR::down_cast<ASR::symbol_t>(ASR::make_Block_t(al,
                statement->base.loc, al.make_new<LCompilers::SymbolTable>(function->m_symtab),
                LCompilers::s2c(al, "inner"), &statement, 1));
            auto* call = ASRUtils::STMT(ASR::make_BlockCall_t(al,
                statement->base.loc, -1, block));
            auto* associate = ASR::down_cast<ASR::symbol_t>(ASR::make_AssociateBlock_t(al,
                statement->base.loc, al.make_new<LCompilers::SymbolTable>(function->m_symtab),
                LCompilers::s2c(al, "outer"), &call, 1));
            auto* outer = ASRUtils::STMT(ASR::make_AssociateBlockCall_t(al,
                statement->base.loc, associate));
            ASR::SideEffectFinder finder;
            finder.visit_stmt(*outer);
            CHECK(finder.found);
            CHECK(finder.loc.first == statement->base.loc.first);
            CHECK(finder.description.find("unchecked dynamic lifecycle effects")
                != std::string::npos);
        }
    }
    SUBCASE("a copy cannot change the declared contract") {
        from_view->m_value = function->m_args[1];
        rejects("asr.verify.trait_owner.contract");
    }
    SUBCASE("owner null state cannot contain a borrowed initializer") {
        owner->m_symbolic_value = function->m_args[0];
        rejects("asr.verify.trait_view.borrowed_storage");
    }
    SUBCASE("a local cannot forge borrowed storage") {
        owner->m_type = ASRUtils::extract_type(owner->m_type);
        rejects("asr.verify.trait_view.borrowed_storage");
    }
    SUBCASE("rejected escaping storage does not poison a later local owner") {
        for (const std::string bad : {
                "subroutine bad(owner)\nclass(IValue), allocatable, optional, intent(out) :: owner\nend subroutine\n",
                "pure function bad() result(owner)\nclass(IValue), allocatable :: owner\nend function\n"}) {
            std::string recovering_source = source.substr(0, source.find("subroutine exercise")) +
                bad + "subroutine good()\nclass(IValue), allocatable :: owner\n"
                "allocate(Payload :: owner)\nend subroutine\nend module\n";
            LCompilers::diag::Diagnostics errors;
            options.continue_compilation = true;
            auto ast = LCompilers::LFortran::parse(al, recovering_source, errors, options);
            REQUIRE(ast.ok);
            auto recovered = LCompilers::LFortran::ast_to_asr(
                al, *ast.result, errors, nullptr, false, options, lm);
            INFO(errors.render2());
            REQUIRE(recovered.ok);
            CHECK(errors.has_error());
            auto *recovered_module = ASR::down_cast<ASR::Module_t>(
                recovered.result->m_symtab->get_symbol("owned_trait_serialization_m"));
            auto *good_symbol = recovered_module->m_symtab->get_symbol("good");
            REQUIRE(good_symbol);
            auto *good = ASR::down_cast<ASR::Function_t>(good_symbol);
            REQUIRE(good->n_body == 1);
            auto *local = ASR::down_cast<ASR::Variable_t>(good->m_symtab->get_symbol("owner"));
            CHECK(local->m_intent == ASR::intentType::Local);
            LCompilers::diag::Diagnostics valid;
            CHECK(LCompilers::asr_verify(*recovered.result, true, valid));
        }
    }
}

TEST_CASE("Runtime trait allocation slots retain canonical association and lifetime") {
    namespace ASR = LCompilers::ASR;
    namespace ASRUtils = LCompilers::ASRUtils;
    const std::string source = R"(
module trait_slot_serialization_m
abstract interface :: IValue
    function value() result(r)
        integer :: r
    end function
end interface
abstract interface, extends(IValue) :: IChild
end interface
type :: Payload
    integer :: n = 5
end type
implements IValue :: Payload
    procedure, pass :: value => get
end implements
contains
integer function get(self)
    class(Payload), intent(in) :: self
    get = self%n
end function
subroutine observe(view)
    class(IValue), intent(in) :: view
end subroutine
subroutine read_slot(slot)
    class(IValue), allocatable, intent(in) :: slot
    if (allocated(slot)) call observe(slot)
end subroutine
subroutine write_slot(slot)
    class(IValue), allocatable, intent(inout) :: slot
    allocate(Payload :: slot)
    slot = Payload(7)
    deallocate(slot)
end subroutine
subroutine out_slot(slot)
    class(IValue), allocatable, intent(out) :: slot
end subroutine
subroutine client(slot, child, view)
    class(IValue), allocatable :: slot
    class(IChild), allocatable, intent(in) :: child
    class(IValue), intent(in) :: view
    call read_slot(slot)
    call write_slot(slot)
    call out_slot(slot)
end subroutine
end module
)";
    ast_ser(source);
    asr_ser(source);
    asr_mod(source);
    Allocator al(1024 * 1024);
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::CompilerOptions options;
    auto parsed = LCompilers::LFortran::parse(al, source, diagnostics, options);
    REQUIRE(parsed.ok);
    LCompilers::LocationManager lm;
    auto result = LCompilers::LFortran::ast_to_asr(
        al, *parsed.result, diagnostics, nullptr, false, options, lm);
    INFO(diagnostics.render2());
    REQUIRE(result.ok);
    auto *module = ASR::down_cast<ASR::Module_t>(
        result.result->m_symtab->get_symbol("trait_slot_serialization_m"));
    auto *client = ASR::down_cast<ASR::Function_t>(
        module->m_symtab->get_symbol("client"));
    auto *write = ASR::down_cast<ASR::Function_t>(
        module->m_symtab->get_symbol("write_slot"));
    auto *call = ASR::down_cast<ASR::SubroutineCall_t>(client->m_body[0]);
    CHECK(ASR::is_a<ASR::Var_t>(*call->m_args[0].m_value));
    CHECK(ASRUtils::EXPR2VAR(client->m_args[0])->m_intent == ASR::intentType::Unspecified);
    auto rejects = [&](const std::string &code) {
        LCompilers::diag::Diagnostics invalid;
        CHECK_FALSE(LCompilers::asr_verify(*result.result, true, invalid));
        INFO(invalid.render2());
        REQUIRE(!invalid.diagnostics.empty());
        CHECK(invalid.diagnostics.back().code == code);
    };
    SUBCASE("named and positional text retain slot qualifiers and intents") {
        for (auto form : {LCompilers::ASRTextForm::Named, LCompilers::ASRTextForm::Positional}) {
            LCompilers::ASRTextOptions text_options;
            text_options.form = form;
            auto text = LCompilers::asr_to_text(*result.result, text_options);
            LCompilers::diag::Diagnostics loaded_diagnostics;
            LCompilers::LocationManager loaded_lm;
            auto loaded = LCompilers::asr_from_text(al, text, "trait_slots.asr",
                loaded_lm, loaded_diagnostics);
            INFO(loaded_diagnostics.render2());
            REQUIRE(loaded.ok);
            CHECK(LCompilers::asr_verify(*loaded.result, true, loaded_diagnostics));
            CHECK(text == LCompilers::asr_to_text(*loaded.result, text_options));
        }
    }
    SUBCASE("even an input slot is invariant") {
        call->m_args[0].m_value = client->m_args[1];
        rejects("asr.verify.trait_owner.argument");
    }
    SUBCASE("a borrowed view is not an allocation slot") {
        call->m_args[0].m_value = client->m_args[2];
        rejects("asr.verify.trait_owner.argument");
    }
    SUBCASE("input slots cannot be allocated, assigned or deallocated") {
        ASRUtils::EXPR2VAR(write->m_args[0])->m_intent = ASR::intentType::In;
        auto **body = write->m_body;
        for (size_t i = 0; i < 3; i++) {
            auto *statement = body[i];
            write->m_body = &statement;
            write->n_body = 1;
            rejects("asr.verify.trait_owner.definable");
        }
    }
    SUBCASE("scope cleanup cannot own the caller's slot") {
        auto *slot = write->m_args[0];
        auto *cleanup = ASRUtils::STMT(ASR::make_ImplicitDeallocate_t(
            al, slot->base.loc, &slot, 1));
        write->m_body = &cleanup;
        write->n_body = 1;
        rejects("asr.verify.trait_owner.caller_lifetime");
    }
    SUBCASE("implicit output cleanup cannot forge purity") {
        auto *out = ASR::down_cast<ASR::Function_t>(
            module->m_symtab->get_symbol("out_slot"));
        ASRUtils::get_FunctionType(out)->m_pure = true;
        rejects("asr.verify.trait_owner.slot_effects");
    }
    SUBCASE("allocation slots cannot forge a C ABI") {
        ASRUtils::get_FunctionType(write)->m_abi = ASR::abiType::BindC;
        rejects("asr.verify.trait_owner.slot_effects");
    }
}

TEST_CASE("Runtime trait results retain scoped ownership through result lowering") {
    namespace ASR = LCompilers::ASR;
    namespace ASRUtils = LCompilers::ASRUtils;
    const std::string source = R"(
module trait_result_serialization_m
abstract interface :: IValue
    function value() result(r)
        integer :: r
    end function
end interface
type :: Payload
    integer :: n = 17
end type
implements IValue :: Payload
    procedure, pass :: value => get
end implements
contains
integer function get(self)
    class(Payload), intent(in) :: self
    get = self%n
end function
function make(fill) result(object)
    logical, intent(in) :: fill
    class(IValue), allocatable :: object
    if (fill) allocate(Payload :: object)
end function
subroutine observe(view)
    class(IValue), intent(in) :: view
end subroutine
logical function ready(view)
    class(IValue), intent(in) :: view
    ready = view%value() == 17
end function
logical function keep(value)
    logical, intent(in) :: value
    keep = value
end function
subroutine exercise()
    call observe(make(.true.))
    if (keep(ready(make(.true.)))) return
    associate (value => keep(ready(make(.true.))))
        if (.not. value) error stop
    end associate
end subroutine
end module
)";
    ast_ser(source);
    asr_ser(source);
    asr_mod(source);
    Allocator al(1024 * 1024);
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::CompilerOptions options;
    auto parsed = LCompilers::LFortran::parse(al, source, diagnostics, options);
    REQUIRE(parsed.ok);
    LCompilers::LocationManager lm;
    auto result = LCompilers::LFortran::ast_to_asr(
        al, *parsed.result, diagnostics, nullptr, false, options, lm);
    INFO(diagnostics.render2());
    REQUIRE(result.ok);
    auto *module = ASR::down_cast<ASR::Module_t>(
        result.result->m_symtab->get_symbol("trait_result_serialization_m"));
    auto *factory = ASR::down_cast<ASR::Function_t>(
        module->m_symtab->get_symbol("make"));
    auto *exercise = ASR::down_cast<ASR::Function_t>(
        module->m_symtab->get_symbol("exercise"));
    auto *return_var = ASRUtils::EXPR2VAR(factory->m_return_var);
    REQUIRE(exercise->n_body == 3);
    CHECK(return_var->m_intent == ASR::intentType::ReturnVar);
    LCompilers::pass_function_result_scope(al, *result.result, options.po);
    REQUIRE(LCompilers::asr_verify(*result.result, true, diagnostics));
    auto *scope_call = ASR::down_cast<ASR::BlockCall_t>(exercise->m_body[0]);
    auto *block = ASR::down_cast<ASR::Block_t>(scope_call->m_m);
    REQUIRE(block->n_body == 2);
    auto *capture = ASR::down_cast<ASR::Assignment_t>(block->m_body[0]);
    auto *call = ASR::down_cast<ASR::FunctionCall_t>(capture->m_value);
    auto *temporary = ASRUtils::EXPR2VAR(capture->m_target);
    CHECK(capture->m_move_allocation);
    CHECK(ASRUtils::is_trait_owner(temporary->m_type));
    CHECK(temporary->m_parent_symtab == block->m_symtab);
    auto rejects = [&](const std::string &code) {
        LCompilers::diag::Diagnostics invalid;
        CHECK_FALSE(LCompilers::asr_verify(*result.result, true, invalid));
        INFO(invalid.render2());
        REQUIRE(!invalid.diagnostics.empty());
        CHECK(invalid.diagnostics.back().code == code);
    };
    SUBCASE("scoped capture survives named and positional serialization") {
        for (auto form : {LCompilers::ASRTextForm::Named, LCompilers::ASRTextForm::Positional}) {
            LCompilers::ASRTextOptions text_options;
            text_options.form = form;
            auto text = LCompilers::asr_to_text(*result.result, text_options);
            LCompilers::diag::Diagnostics loaded_diagnostics;
            LCompilers::LocationManager loaded_lm;
            auto loaded = LCompilers::asr_from_text(al, text, "trait_results.asr",
                loaded_lm, loaded_diagnostics);
            INFO(loaded_diagnostics.render2());
            REQUIRE(loaded.ok);
            CHECK(LCompilers::asr_verify(*loaded.result, true, loaded_diagnostics));
            CHECK(text == LCompilers::asr_to_text(*loaded.result, text_options));
        }
        SUBCASE("result scope re-entry does not recapture an owned temporary") {
            auto before = LCompilers::asr_to_text(*result.result);
            LCompilers::pass_function_result_scope(al, *result.result, options.po);
            CHECK(LCompilers::asr_verify(*result.result, true, diagnostics));
            CHECK(before == LCompilers::asr_to_text(*result.result));
        }
        SUBCASE("imported explicit factories retain the Fortran result ABI") {
            module->m_symtab->mark_all_variables_external(al);
            CHECK(ASRUtils::get_FunctionType(factory)->m_abi == ASR::abiType::ExternalUndefined);
            CHECK(LCompilers::asr_verify(*result.result, true, diagnostics));
        }
    }
    SUBCASE("capture becomes a shared OUT-slot call, not a pointer or value copy") {
        LCompilers::pass_create_subroutine_from_function(al, *result.result, options.po);
        CHECK(LCompilers::asr_verify(*result.result, true, diagnostics));
        CHECK(factory->m_return_var == nullptr);
        CHECK(return_var->m_intent == ASR::intentType::Out);
        CHECK(factory->n_args == 2);
        bool saw_call = false;
        for (size_t i = 0; i < block->n_body; i++) {
            CHECK_FALSE(ASR::is_a<ASR::Assignment_t>(*block->m_body[i]));
            CHECK_FALSE(ASR::is_a<ASR::Associate_t>(*block->m_body[i]));
            if (!ASR::is_a<ASR::SubroutineCall_t>(*block->m_body[i])) continue;
            auto *lowered = ASR::down_cast<ASR::SubroutineCall_t>(block->m_body[i]);
            if (ASRUtils::symbol_get_past_external(lowered->m_name) != &factory->base) continue;
            saw_call = true;
            REQUIRE(lowered->n_args == 2);
            CHECK(ASRUtils::EXPR2VAR(lowered->m_args[1].m_value) == temporary);
        }
        CHECK(saw_call);
        LCompilers::pass_intent_out_deallocate(al, *result.result, options.po);
        CHECK(LCompilers::asr_verify(*result.result, true, diagnostics));
        REQUIRE(ASR::is_a<ASR::If_t>(*factory->m_body[0]));
        auto *entry = ASR::down_cast<ASR::If_t>(factory->m_body[0]);
        REQUIRE(entry->n_body == 1);
        REQUIRE(ASR::is_a<ASR::ExplicitDeallocate_t>(*entry->m_body[0]));
    }
    SUBCASE("ordinary assignment cannot masquerade as capture") {
        capture->m_move_allocation = false;
        rejects("asr.verify.trait_owner.value_copy");
    }
    SUBCASE("a move cannot steal another variable's allocation") {
        capture->m_value = capture->m_target;
        rejects("asr.verify.trait_owner.value_copy");
    }
    SUBCASE("capture cannot escape through SAVE") {
        temporary->m_storage = ASR::storage_typeType::Save;
        rejects("asr.verify.trait_owner.value_copy");
    }
    SUBCASE("a factory result cannot be a borrowed header") {
        call->m_type = ASRUtils::extract_type(call->m_type);
        rejects("asr.verify.trait_owner.value_copy");
    }
    SUBCASE("callee scope cleanup cannot destroy the actual returned result") {
        auto *value = factory->m_return_var;
        auto *cleanup = ASRUtils::STMT(ASR::make_ImplicitDeallocate_t(
            al, value->base.loc, &value, 1));
        factory->m_body = &cleanup;
        factory->n_body = 1;
        rejects("asr.verify.trait_owner.caller_lifetime");
    }
    SUBCASE("result cleanup effects remain visible through an executable block") {
        factory->m_side_effect_free = true;
        ASR::SideEffectFinder finder;
        finder.visit_stmt(*exercise->m_body[0]);
        CHECK(finder.found);
        CHECK(finder.description.find("unchecked dynamic lifecycle effects") != std::string::npos);
    }
    SUBCASE("a result declaration cannot forge purity") {
        ASRUtils::get_FunctionType(factory)->m_pure = true;
        rejects("asr.verify.trait_owner.slot_effects");
    }
}

TEST_CASE("Inherited assignment retains concrete or dynamic binding") {
    namespace ASR = LCompilers::ASR;
    namespace ASRUtils = LCompilers::ASRUtils;
    const std::string source = R"(
module inherited_assignment_m
type, abstract :: base
contains
    procedure(signature), deferred :: assign
    generic :: assignment(=) => assign
end type
abstract interface
    subroutine signature(self, other)
        import base
        class(base), intent(inout) :: self
        class(base), intent(in) :: other
    end subroutine
end interface
type, extends(base) :: child
contains
    procedure :: assign => assign_child
end type
type, extends(child) :: descendant
end type
contains
subroutine assign_child(self, other)
    class(child), intent(inout) :: self
    class(base), intent(in) :: other
end subroutine
end module
)";
    asr_ser(source);
    asr_mod(source);
    Allocator al(1024 * 1024);
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::CompilerOptions options;
    auto parsed = LCompilers::LFortran::parse(al, source, diagnostics, options);
    REQUIRE(parsed.ok);
    LCompilers::LocationManager lm;
    auto result = LCompilers::LFortran::ast_to_asr(
        al, *parsed.result, diagnostics, nullptr, false, options, lm);
    INFO(diagnostics.render2());
    REQUIRE(result.ok);
    auto* scope = ASR::down_cast<ASR::Module_t>(
        result.result->m_symtab->get_symbol("inherited_assignment_m"))->m_symtab;
    auto* base = ASR::down_cast<ASR::Struct_t>(scope->get_symbol("base"));
    auto* child = ASR::down_cast<ASR::Struct_t>(scope->get_symbol("child"));
    auto* descendant = ASR::down_cast<ASR::Struct_t>(scope->get_symbol("descendant"));
    auto* override = ASR::down_cast<ASR::Function_t>(scope->get_symbol("assign_child"));
    auto* binding = ASR::down_cast<ASR::StructMethodDeclaration_t>(
        child->m_symtab->get_symbol("assign"));
    auto exact = ASRUtils::resolve_struct_defined_assignment(child, false);
    CHECK(exact.procedure == override);
    CHECK(exact.dispatch_binding == nullptr);
    auto inherited = ASRUtils::resolve_struct_defined_assignment(descendant, false);
    CHECK(inherited.procedure == override);
    CHECK(inherited.dispatch_binding == nullptr);
    auto dynamic = ASRUtils::resolve_struct_defined_assignment(child, true);
    CHECK(dynamic.procedure == override);
    CHECK(dynamic.dispatch_binding == binding);
    auto abstract = ASRUtils::resolve_struct_defined_assignment(base, false);
    CHECK(abstract.procedure == nullptr);
    CHECK(abstract.dispatch_binding == nullptr);
    auto deferred = ASRUtils::resolve_struct_defined_assignment(base, true);
    CHECK(deferred.procedure == ASR::down_cast<ASR::Function_t>(
        scope->get_symbol("signature")));
    REQUIRE(deferred.dispatch_binding);
    CHECK(deferred.dispatch_binding->m_is_deferred);
}

TEST_CASE("Runtime trait combinations report their declaration boundary") {
    const std::string contracts = R"(
module runtime_combo_contracts
abstract interface :: A
    function value() result(r)
        integer :: r
    end function
end interface
abstract interface :: B
    function other() result(r)
        integer :: r
    end function
end interface
end module
)";
    for (const auto &declaration : {"program combined\n", "subroutine combined()\n",
            "function combined() result(r)\n"}) {
        CAPTURE(declaration);
        const std::string source = contracts + declaration +
            "use runtime_combo_contracts\nimplicit none\n"
            "class(A + B), pointer :: view\ninteger :: r\nend\n";
        Allocator al(1024 * 1024);
        LCompilers::diag::Diagnostics diagnostics;
        LCompilers::CompilerOptions options;
        LCompilers::LocationManager lm;
        auto parsed = LCompilers::LFortran::parse(al, source, diagnostics, options);
        REQUIRE(parsed.ok);
        auto result = LCompilers::LFortran::ast_to_asr(
            al, *parsed.result, diagnostics, nullptr, false, options, lm);
        CHECK_FALSE(result.ok);
        CHECK(diagnostics.render2().find(
            "runtime trait combinations are not implemented yet") != std::string::npos);
    }
}

TEST_CASE("Runtime trait evidence is checked before its defining module") {
    namespace ASR = LCompilers::ASR;
    const std::string source = runtime_trait_serialization_source + R"(
program a_runtime_trait_consumer
    use runtime_trait_serialization_m
    type(Payload) :: object
    object%n = 19
    if (observe(object) /= 19) error stop
end program
)";
    Allocator al(1024 * 1024);
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::CompilerOptions options;
    auto parsed = LCompilers::LFortran::parse(al, source, diagnostics, options);
    REQUIRE(parsed.ok);
    LCompilers::LocationManager lm;
    auto result = LCompilers::LFortran::ast_to_asr(
        al, *parsed.result, diagnostics, nullptr, false, options, lm);
    INFO(diagnostics.render2());
    REQUIRE(result.ok);
    CHECK(LCompilers::asr_verify(*result.result, true, diagnostics));
    auto *scope = result.result->m_symtab;
    REQUIRE(ASR::is_a<ASR::Program_t>(*scope->get_scope().begin()->second));
    auto *module = ASR::down_cast<ASR::Module_t>(
        scope->get_symbol("runtime_trait_serialization_m"));
    ASR::TraitWitness_t *witness = nullptr;
    for (auto &entry : module->m_symtab->get_scope()) {
        if (ASR::is_a<ASR::TraitWitness_t>(*entry.second)) {
            witness = ASR::down_cast<ASR::TraitWitness_t>(entry.second);
        }
    }
    REQUIRE(witness);
    std::string code;
    SUBCASE("a pack checks the implementation kind before the witness is visited") {
        witness->m_implementation = module->m_symtab->get_symbol("payload");
        code = "asr.verify.trait_witness.evidence";
    }
    SUBCASE("a forward witness reference checks its procedure records") {
        witness->m_procedures = nullptr;
        code = "asr.verify.trait_witness.complete";
    }
    SUBCASE("a consumer rejects a changed receiver contract") {
        auto *observe = ASR::down_cast<ASR::Function_t>(
            module->m_symtab->get_symbol("observe"));
        auto *view = ASR::down_cast<ASR::TraitObjectType_t>(
            LCompilers::ASRUtils::expr_type(observe->m_args[0]));
        view->m_contract = module->m_symtab->get_symbol("ivalue");
        code = "asr.verify.trait_owner.argument";
    }
    LCompilers::diag::Diagnostics invalid;
    CHECK_FALSE(LCompilers::asr_verify(*result.result, true, invalid));
    INFO(invalid.render2());
    REQUIRE(!invalid.diagnostics.empty());
    CHECK(invalid.diagnostics.back().code == code);
}

TEST_CASE("Runtime trait dummies validate completed procedure attributes") {
    const std::string contracts = R"(
module runtime_attributes_m
abstract interface :: IValue
    function value() result(r)
        integer :: r
    end function
end interface
contains
)";
    for (bool function : {false, true}) {
        for (bool recovery : {false, true}) {
            for (const std::string attribute : {
                    "optional :: object", "allocatable :: object",
                    "pointer :: object", "save :: object",
                    "dimension :: object(2)", "value :: object",
                    "intent(out) :: object"}) {
                Allocator al(1024 * 1024);
                LCompilers::diag::Diagnostics diagnostics;
                LCompilers::CompilerOptions options;
                options.continue_compilation = recovery;
                options.implicit_interface = true;
                std::string source = contracts +
                    (function ? "function consume(object) result(r)\n"
                              : "subroutine consume(object)\n") +
                    "class(IValue), intent(in) :: object\n" + attribute + "\n" +
                    (function ? "integer :: r\nr = 0\nend function\n"
                              : "end subroutine\n") + "end module\n";
                INFO(source);
                auto parsed = LCompilers::LFortran::parse(al, source, diagnostics, options);
                REQUIRE(parsed.ok);
                LCompilers::LocationManager lm;
                auto result = LCompilers::LFortran::ast_to_asr(
                    al, *parsed.result, diagnostics, nullptr, false, options, lm);
                INFO(diagnostics.render2());
                bool valid_slot = attribute == "allocatable :: object";
                if (valid_slot) {
                    CHECK(result.ok);
                    CHECK_FALSE(diagnostics.has_error());
                } else {
                    if (!recovery) CHECK_FALSE(result.ok);
                    CHECK(diagnostics.has_error());
                }
                if (result.ok) {
                    LCompilers::diag::Diagnostics valid;
                    CHECK(LCompilers::asr_verify(*result.result, true, valid));
                    INFO(valid.render2());
                }
                for (const auto &diagnostic : diagnostics.diagnostics) {
                    CHECK(diagnostic.code.find("asr.verify.") != 0);
                }
            }
        }
    }
}

TEST_CASE("Runtime trait slots retain diamonds and independent nominal origins") {
    namespace ASR = LCompilers::ASR;
    namespace ASRUtils = LCompilers::ASRUtils;
    std::string source = R"(
module runtime_origins_m
abstract interface :: IBase
    function value() result(r)
        integer :: r
    end function
end interface
abstract interface, extends(IBase) :: ILeft
end interface
abstract interface, extends(IBase) :: IRight
end interface
abstract interface :: IIndependent
    function value() result(r)
        integer :: r
    end function
end interface
abstract interface, extends(ILeft + IRight + IIndependent) :: ICombined
end interface
end module
)";
    asr_ser(source);
    asr_mod(source);
    Allocator al(1024 * 1024);
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::CompilerOptions options;
    auto parsed = LCompilers::LFortran::parse(al, source, diagnostics, options);
    REQUIRE(parsed.ok);
    LCompilers::LocationManager lm;
    auto result = LCompilers::LFortran::ast_to_asr(
        al, *parsed.result, diagnostics, nullptr, false, options, lm);
    REQUIRE(result.ok);
    auto *module = ASR::down_cast<ASR::Module_t>(
        result.result->m_symtab->get_symbol("runtime_origins_m"));
    auto *contract = ASRUtils::trait_runtime_contract(module->m_symtab->get_symbol("icombined"));
    REQUIRE(contract);
    REQUIRE(contract->n_slots == 1);
    CHECK(contract->m_slots[0].n_origins == 2);
    auto *first = ASRUtils::symbol_get_past_external(contract->m_slots[0].m_origins[0]);
    auto *second = ASRUtils::symbol_get_past_external(contract->m_slots[0].m_origins[1]);
    CHECK(ASRUtils::get_asr_owner(first) == module->m_symtab->get_symbol("ibase"));
    CHECK(ASRUtils::get_asr_owner(second) == module->m_symtab->get_symbol("iindependent"));
    auto printed = LCompilers::LFortran::ast_to_src(*parsed.result);
    ast_ser(printed);
    asr_ser(printed);
    ast_ser("module runtime_combination_m\ncontains\nsubroutine f(x)\n"
        "class(A + B), intent(in) :: x\nend subroutine\nend module\n");
}

TEST_CASE("Trait dependent signature normalization and serialization") {
    namespace ASR = LCompilers::ASR;
    namespace ASRUtils = LCompilers::ASRUtils;
    const std::string source = R"(
module trait_dependent_signature_m
implicit none
abstract interface :: IResult
    pure function text(n, zinput) result(r)
        integer, intent(in) :: n
        character(len=n), intent(in) :: zinput
        character(len=n) :: r
    end function
    pure function values(offset, n, zinput) result(r)
        integer, intent(in) :: offset, n, zinput(n)
        integer :: r(n)
    end function
end interface
contains
function unused{IResult :: T}(x) result(r)
    type(T), intent(in) :: x
    integer :: r
    r = 0
end function
end module
)";
    ast_ser(source);
    asr_ser(source);
    asr_mod(source);

    Allocator al(1024 * 1024);
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::CompilerOptions options;
    auto parsed = LCompilers::LFortran::parse(al, source, diagnostics, options);
    REQUIRE(parsed.ok);
    LCompilers::LocationManager lm;
    auto result = LCompilers::LFortran::ast_to_asr(
        al, *parsed.result, diagnostics, nullptr, false, options, lm);
    REQUIRE(result.ok);
    ASR::Module_t *module = ASR::down_cast<ASR::Module_t>(
        result.result->m_symtab->get_symbol("trait_dependent_signature_m"));
    ASR::Template_t *generic = ASR::down_cast<ASR::Template_t>(
        module->m_symtab->get_symbol("unused"));
    ASR::TraitConstraint_t *constraint = nullptr;
    for (const auto &entry : generic->m_symtab->get_scope()) {
        if (ASR::is_a<ASR::TraitConstraint_t>(*entry.second)) {
            constraint = ASR::down_cast<ASR::TraitConstraint_t>(entry.second);
        }
    }
    REQUIRE(constraint != nullptr);
    REQUIRE(constraint->n_requirements == 2);
    for (size_t i = 0; i < constraint->n_requirements; i++) {
        auto &requirement = constraint->m_requirements[i];
        auto *member = ASR::down_cast<ASR::Function_t>(
            ASRUtils::symbol_get_past_external(requirement.m_member));
        auto *procedure = ASR::down_cast<ASR::Function_t>(requirement.m_procedure);
        auto *original = ASRUtils::get_FunctionType(member);
        auto *signature = ASRUtils::get_FunctionType(procedure);
        CAPTURE(member->m_name);
        REQUIRE(signature->n_arg_types == original->n_arg_types + 1);
        CHECK(ASR::is_a<ASR::TypeParameter_t>(*signature->m_arg_types[0]));
        CHECK(signature->m_abi == original->m_abi);
        CHECK(signature->m_deftype == original->m_deftype);
        CHECK(signature->m_pure);
        CHECK(signature->m_is_restriction);
        CHECK_FALSE(original->m_is_restriction);
        const bool is_text = std::string(member->m_name) == "text";
        const int parameter = is_text ? 1 : 2;
        for (auto *type : {signature->m_return_var_type,
                          signature->m_arg_types[signature->n_arg_types - 1]}) {
            ASR::expr_t *length = is_text
                ? ASR::down_cast<ASR::String_t>(type)->m_len
                : ASR::down_cast<ASR::Array_t>(type)->m_dims[0].m_length;
            REQUIRE(ASR::is_a<ASR::FunctionParam_t>(*length));
            CHECK(ASR::down_cast<ASR::FunctionParam_t>(length)->m_param_number
                == parameter);
        }
        ASR::expr_t *original_length = is_text
            ? ASR::down_cast<ASR::String_t>(original->m_return_var_type)->m_len
            : ASR::down_cast<ASR::Array_t>(original->m_return_var_type)->m_dims[0].m_length;
        REQUIRE(ASR::is_a<ASR::FunctionParam_t>(*original_length));
        CHECK(ASR::down_cast<ASR::FunctionParam_t>(original_length)->m_param_number
            == parameter - 1);
    }
}

TEST_CASE("Trait conformance verification preserves nominal identity") {
    namespace ASR = LCompilers::ASR;
    Allocator al(1024 * 1024);
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::CompilerOptions options;
    auto parsed = LCompilers::LFortran::parse(
        al, trait_serialization_source, diagnostics, options);
    REQUIRE(parsed.ok);
    LCompilers::LocationManager lm;
    auto result = LCompilers::LFortran::ast_to_asr(
        al, *parsed.result, diagnostics, nullptr, false, options, lm);
    INFO(diagnostics.render2());
    REQUIRE(result.ok);
    ASR::Module_t *module = ASR::down_cast<ASR::Module_t>(
        result.result->m_symtab->get_symbol("trait_serialization_m"));
    ASR::TraitImplementation_t *implementation = nullptr;
    for (const auto &entry : module->m_symtab->get_scope()) {
        if (ASR::is_a<ASR::TraitImplementation_t>(*entry.second)) {
            implementation = ASR::down_cast<ASR::TraitImplementation_t>(entry.second);
        }
    }
    REQUIRE(implementation != nullptr);
    REQUIRE(implementation->n_bindings == 1);
    ASR::Trait_t *other = ASR::down_cast<ASR::Trait_t>(
        module->m_symtab->get_symbol("iother"));
    ASR::symbol_t *member = implementation->m_bindings[0].m_member;
    implementation->m_bindings[0].m_member = other->m_symtab->get_symbol("get_value");
    LCompilers::diag::Diagnostics wrong_member;
    CHECK_FALSE(LCompilers::asr_verify(*result.result, true, wrong_member));
    REQUIRE(!wrong_member.diagnostics.empty());
    CHECK(wrong_member.diagnostics.back().code ==
        "asr.verify.trait_implementation.member_belongs_to_trait");
    implementation->m_bindings[0].m_member = member;

    implementation->m_type_declaration = nullptr;
    LCompilers::diag::Diagnostics missing_type;
    CHECK_FALSE(LCompilers::asr_verify(*result.result, true, missing_type));
    REQUIRE(!missing_type.diagnostics.empty());
    CHECK(missing_type.diagnostics.back().code ==
        "asr.verify.trait_implementation.nominal_type_required");

    implementation->m_type_declaration = module->m_symtab->get_symbol("box");
    ASR::Template_t *generic = ASR::down_cast<ASR::Template_t>(
        module->m_symtab->get_symbol("read_value"));
    ASR::TraitConstraint_t *constraint = nullptr;
    for (const auto &entry : generic->m_symtab->get_scope()) {
        if (ASR::is_a<ASR::TraitConstraint_t>(*entry.second)) {
            constraint = ASR::down_cast<ASR::TraitConstraint_t>(entry.second);
        }
    }
    REQUIRE(constraint != nullptr);
    REQUIRE(constraint->n_requirements == 1);
    constraint->m_requirements[0].m_procedure =
        LCompilers::ASRUtils::symbol_get_past_external(
            constraint->m_requirements[0].m_member);
    LCompilers::diag::Diagnostics missing_receiver;
    CHECK_FALSE(LCompilers::asr_verify(*result.result, true, missing_receiver));
    REQUIRE(!missing_receiver.diagnostics.empty());
    CHECK(missing_receiver.diagnostics.back().code ==
        "asr.verify.trait_requirement.normalized_arg_count");
}

static const std::string inherited_trait_serialization_source = R"(
module z_trait_contracts
implicit none
abstract interface :: IBase
    pure function get_value(step) result(value)
        integer, intent(in), optional :: step
        integer :: value
    end function
end interface
abstract interface :: IOther
    pure function get_value(step) result(other_value)
        integer, intent(in), optional :: step
        integer :: other_value
    end function
end interface
abstract interface :: IUnrelated
    pure function get_value(step) result(value)
        integer, intent(in), optional :: step
        integer :: value
    end function
end interface
end module
module a_inherited_traits
use z_trait_contracts, only: Root => IBase, IOther, IUnrelated
implicit none
abstract interface, extends(Root) :: Left
    function left_value() result(value)
        integer :: value
    end function
end interface
abstract interface, extends(Root) :: Right
end interface
abstract interface, extends(Left + Right + IOther) :: Child
end interface
type :: Box
    integer :: value
end type
type :: Twin
    integer :: value
end type
type(Box) :: first
type(Twin) :: second
implements Child :: Box
    procedure, pass(self) :: get_value => box_value
    procedure, nopass :: left_value => constant_value
end implements
implements Root :: Box
    procedure, pass(self) :: get_value => box_value
end implements
implements Root :: Twin
    procedure, pass(self) :: get_value => twin_value
end implements
contains
pure function box_value(step, self) result(value)
    integer, intent(in), optional :: step
    class(Box), intent(in) :: self
    integer :: value
    value = self%value
    if (present(step)) value = value + step
end function
pure function twin_value(step, self) result(value)
    integer, intent(in), optional :: step
    class(Twin), intent(in) :: self
    integer :: value
    value = 2 * self%value
    if (present(step)) value = value + step
end function
function constant_value() result(value)
    integer :: value
    value = 1
end function
function read_root{Root :: V}(object) result(value)
    type(V), intent(in) :: object
    integer :: value
    value = object%get_value()
end function
function read_child{Child :: T}(object) result(value)
    type(T), intent(in) :: object
    integer :: value
    value = read_root(object) + read_root{T}(object) + object%left_value()
end function
function read_pair{Root :: V, Root :: W}(a, b) result(value)
    type(V), intent(in) :: a
    type(W), intent(in) :: b
    integer :: value
    value = read_root(a) + 10 * read_root(b)
end function
function read_values{Child + Root + IOther :: T, Root :: U}(a, b) result(value)
    type(T), intent(in) :: a
    type(U), intent(in) :: b
    integer :: value
    value = a%get_value(step=1) + a%left_value() + b%get_value() &
        + read_child(a) + read_pair(b, a)
end function
function concrete_driver() result(value)
    type(Box) :: a
    type(Twin) :: b
    integer :: value
    a = Box(3)
    b = Twin(5)
    value = read_values(a, b) + read_values{Box, Twin}(a, b)
end function
end module
)";

TEST_CASE("Inherited traits serialize canonical members and parent imports") {
    ast_ser(inherited_trait_serialization_source);
    asr_ser(inherited_trait_serialization_source);
    asr_mod(inherited_trait_serialization_source, "a_inherited_traits");
}

TEST_CASE("Inherited trait contracts retain nominal obligations") {
    namespace ASR = LCompilers::ASR;
    namespace ASRUtils = LCompilers::ASRUtils;
    Allocator al(1024 * 1024);
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::CompilerOptions options;
    auto parsed = LCompilers::LFortran::parse(
        al, inherited_trait_serialization_source, diagnostics, options);
    REQUIRE(parsed.ok);
    LCompilers::LocationManager lm;
    auto result = LCompilers::LFortran::ast_to_asr(
        al, *parsed.result, diagnostics, nullptr, false, options, lm);
    INFO(diagnostics.render2());
    REQUIRE(result.ok);
    auto *module = ASR::down_cast<ASR::Module_t>(
        result.result->m_symtab->get_symbol("a_inherited_traits"));
    auto *contracts = ASR::down_cast<ASR::Module_t>(
        result.result->m_symtab->get_symbol("z_trait_contracts"));
    auto *child = ASR::down_cast<ASR::Trait_t>(module->m_symtab->get_symbol("child"));
    auto *base = ASR::down_cast<ASR::Trait_t>(contracts->m_symtab->get_symbol("ibase"));
    auto *other = ASR::down_cast<ASR::Trait_t>(contracts->m_symtab->get_symbol("iother"));
    auto *unrelated = ASR::down_cast<ASR::Trait_t>(
        contracts->m_symtab->get_symbol("iunrelated"));
    auto *generic = ASR::down_cast<ASR::Template_t>(
        module->m_symtab->get_symbol("read_values"));
    auto constraint_for = [&](const std::string &parameter, ASR::Trait_t *trait) {
        ASR::TraitConstraint_t *found = nullptr;
        for (const auto &entry : generic->m_symtab->get_scope()) {
            if (!ASR::is_a<ASR::TraitConstraint_t>(*entry.second)) continue;
            auto *constraint = ASR::down_cast<ASR::TraitConstraint_t>(entry.second);
            if (parameter == ASRUtils::symbol_name(constraint->m_parameter) &&
                    ASRUtils::symbol_get_past_external(constraint->m_trait) == &trait->base) {
                found = constraint;
            }
        }
        return found;
    };
    auto *child_constraint = constraint_for("t", child);
    auto *base_constraint = constraint_for("t", base);
    auto *other_constraint = constraint_for("t", other);
    auto *independent_constraint = constraint_for("u", base);
    REQUIRE(child_constraint != nullptr);
    REQUIRE(base_constraint != nullptr);
    REQUIRE(other_constraint != nullptr);
    REQUIRE(independent_constraint != nullptr);
    ASR::TraitImplementation_t *implementation = nullptr;
    for (const auto &entry : module->m_symtab->get_scope()) {
        if (!ASR::is_a<ASR::TraitImplementation_t>(*entry.second)) continue;
        auto *candidate = ASR::down_cast<ASR::TraitImplementation_t>(entry.second);
        if (ASRUtils::symbol_get_past_external(candidate->m_trait) == &child->base) {
            implementation = candidate;
        }
    }
    REQUIRE(implementation != nullptr);
    REQUIRE(implementation->n_bindings == 3);
    REQUIRE(child_constraint->n_requirements == 3);
    REQUIRE(base_constraint->n_requirements == 1);
    REQUIRE(other_constraint->n_requirements == 1);
    REQUIRE(independent_constraint->n_requirements == 1);

    SUBCASE("diamond originals and distinct same-name contracts") {
        auto hierarchy = ASRUtils::trait_hierarchy(*child);
        CHECK(hierarchy.error == ASRUtils::TraitHierarchyError::None);
        CHECK(hierarchy.traits.size() == 5);
        REQUIRE(hierarchy.members.size() == 3);
        CHECK(hierarchy.members[0] == base->m_symtab->get_symbol("get_value"));
        CHECK(hierarchy.members[2] == other->m_symtab->get_symbol("get_value"));
        CHECK(base_constraint->m_requirements[0].m_procedure ==
            other_constraint->m_requirements[0].m_procedure);
        CHECK(base_constraint->m_requirements[0].m_procedure !=
            independent_constraint->m_requirements[0].m_procedure);
        for (size_t i = 0; i < child_constraint->n_requirements; i++) {
            const auto &requirement = child_constraint->m_requirements[i];
            REQUIRE(ASR::is_a<ASR::ExternalSymbol_t>(*requirement.m_member));
            auto *reference = ASR::down_cast<ASR::ExternalSymbol_t>(requirement.m_member);
            REQUIRE(reference->n_scope_names == 1);
            auto *owner = ASRUtils::get_asr_owner(reference->m_external);
            CHECK(std::string(reference->m_scope_names[0]) == ASRUtils::symbol_name(owner));
            CHECK(std::string(reference->m_module_name) ==
                ASRUtils::symbol_name(ASRUtils::get_asr_owner(owner)));
            if (std::string(ASRUtils::symbol_name(reference->m_external)) == "get_value") {
                CHECK(requirement.m_procedure == base_constraint->m_requirements[0].m_procedure);
            }
        }
        REQUIRE(child->n_parents == 3);
        CHECK(child->m_parents[0] == module->m_symtab->get_symbol("left"));
        CHECK(child->m_parents[1] == module->m_symtab->get_symbol("right"));
        CHECK(child->m_parents[2] == module->m_symtab->get_symbol("iother"));
    }

    SUBCASE("same-layout ordinary types remain nominally distinct") {
        auto *a = ASRUtils::EXPR(ASR::make_Var_t(
            al, module->base.base.loc, module->m_symtab->get_symbol("first")));
        auto *b = ASRUtils::EXPR(ASR::make_Var_t(
            al, module->base.base.loc, module->m_symtab->get_symbol("second")));
        CHECK_FALSE(ASRUtils::trait_types_equal(a, b));
    }

    SUBCASE("contract equality includes dummy names and procedure attributes") {
        auto *a = ASR::down_cast<ASR::Function_t>(
            base->m_symtab->get_symbol("get_value"));
        auto *b = ASR::down_cast<ASR::Function_t>(
            other->m_symtab->get_symbol("get_value"));
        CHECK(ASRUtils::trait_method_mismatch(*a, *b).difference ==
            ASRUtils::TraitMethodDifference::None);
        auto *arg = ASRUtils::EXPR2VAR(b->m_args[0]);
        char *name = arg->m_name;
        arg->m_name = LCompilers::s2c(al, "different_name");
        CHECK(ASRUtils::trait_method_mismatch(*a, *b).difference ==
            ASRUtils::TraitMethodDifference::Contract);
        arg->m_name = name;
        arg->m_intent = ASR::intentType::Out;
        CHECK(ASRUtils::trait_method_mismatch(*a, *b).difference ==
            ASRUtils::TraitMethodDifference::Contract);
        arg->m_intent = ASR::intentType::In;
        arg->m_presence = ASR::presenceType::Required;
        CHECK(ASRUtils::trait_method_mismatch(*a, *b).difference ==
            ASRUtils::TraitMethodDifference::Contract);
        arg->m_presence = ASR::presenceType::Optional;
        for (bool *attribute : {&arg->m_value_attr, &arg->m_target_attr,
                &arg->m_contiguous_attr, &arg->m_is_volatile}) {
            *attribute = true;
            CHECK(ASRUtils::trait_method_mismatch(*a, *b).difference ==
                ASRUtils::TraitMethodDifference::Contract);
            *attribute = false;
        }
        auto *signature = ASRUtils::get_FunctionType(b);
        signature->m_pure = false;
        CHECK(ASRUtils::trait_method_mismatch(*a, *b).difference ==
            ASRUtils::TraitMethodDifference::Contract);
        signature->m_pure = true;
        signature->m_elemental = true;
        CHECK(ASRUtils::trait_method_mismatch(*a, *b).difference ==
            ASRUtils::TraitMethodDifference::Contract);
        signature->m_elemental = false;
        signature->m_abi = ASR::abiType::BindC;
        CHECK(ASRUtils::trait_method_mismatch(*a, *b).difference ==
            ASRUtils::TraitMethodDifference::Contract);
    }

    SUBCASE("argument overloads differ from incompatible result contracts") {
        auto *a = ASR::down_cast<ASR::Function_t>(
            base->m_symtab->get_symbol("get_value"));
        auto *b = ASR::down_cast<ASR::Function_t>(
            other->m_symtab->get_symbol("get_value"));
        auto *arg = ASRUtils::EXPR2VAR(b->m_args[0]);
        ASR::ttype_t *type = arg->m_type;
        arg->m_type = ASRUtils::TYPE(ASR::make_Integer_t(al, type->base.loc, 8));
        CHECK(ASRUtils::trait_method_mismatch(*a, *b).difference ==
            ASRUtils::TraitMethodDifference::Arguments);
        ASR::dimension_t dimension;
        dimension.loc = type->base.loc;
        dimension.m_start = nullptr;
        dimension.m_length = nullptr;
        arg->m_type = ASRUtils::TYPE(ASR::make_Array_t(al, type->base.loc, type,
            &dimension, 1, ASR::array_physical_typeType::DescriptorArray,
            ASR::memory_spaceType::Global));
        CHECK(ASRUtils::trait_method_mismatch(*a, *b).difference ==
            ASRUtils::TraitMethodDifference::Arguments);
        arg->m_type = type;
        auto *value = ASRUtils::EXPR2VAR(b->m_return_var);
        value->m_type = ASRUtils::TYPE(ASR::make_Real_t(al, type->base.loc, 8));
        CHECK(ASRUtils::trait_method_mismatch(*a, *b).difference ==
            ASRUtils::TraitMethodDifference::Contract);
    }

    SUBCASE("character contracts compare corresponding dummy lengths") {
        auto *a = ASR::down_cast<ASR::Function_t>(
            base->m_symtab->get_symbol("get_value"));
        auto *b = ASR::down_cast<ASR::Function_t>(
            other->m_symtab->get_symbol("get_value"));
        for (auto *procedure : {a, b}) {
            auto *value = ASRUtils::EXPR2VAR(procedure->m_return_var);
            value->m_type = ASRUtils::TYPE(ASR::make_String_t(al, value->base.base.loc,
                1, procedure->m_args[0], ASR::string_length_kindType::ExpressionLength,
                ASR::string_physical_typeType::DescriptorString));
        }
        CHECK(ASRUtils::trait_method_mismatch(*a, *b).difference ==
            ASRUtils::TraitMethodDifference::None);
        auto *value = ASRUtils::EXPR2VAR(b->m_return_var);
        auto *type = ASR::down_cast<ASR::String_t>(value->m_type);
        type->m_len = ASRUtils::EXPR(ASR::make_IntegerConstant_t(al, type->base.base.loc,
            2, ASRUtils::TYPE(ASR::make_Integer_t(al, type->base.base.loc, 4))));
        CHECK(ASRUtils::trait_method_mismatch(*a, *b).difference ==
            ASRUtils::TraitMethodDifference::Contract);
    }

    SUBCASE("a cycle is rejected rather than truncated") {
        base->m_parents = al.allocate<ASR::symbol_t*>(1);
        base->m_parents[0] = &base->base;
        base->n_parents = 1;
        LCompilers::diag::Diagnostics invalid;
        CHECK_FALSE(LCompilers::asr_verify(*result.result, true, invalid));
        REQUIRE(!invalid.diagnostics.empty());
        CHECK(invalid.diagnostics.back().code == "asr.verify.trait.inheritance_cycle");
    }

    SUBCASE("array result contracts compare corresponding extents") {
        auto *a = ASR::down_cast<ASR::Function_t>(
            base->m_symtab->get_symbol("get_value"));
        auto *b = ASR::down_cast<ASR::Function_t>(
            other->m_symtab->get_symbol("get_value"));
        for (auto *procedure : {a, b}) {
            auto *value = ASRUtils::EXPR2VAR(procedure->m_return_var);
            ASRUtils::EXPR2VAR(procedure->m_args[0])->m_presence = ASR::presenceType::Required;
            auto *dimension = al.allocate<ASR::dimension_t>(1);
            dimension->loc = value->base.base.loc;
            dimension->m_start = nullptr;
            dimension->m_length = procedure->m_args[0];
            value->m_type = ASRUtils::TYPE(ASR::make_Array_t(al, value->base.base.loc,
                value->m_type, dimension, 1, ASR::array_physical_typeType::DescriptorArray,
                ASR::memory_spaceType::Global));
        }
        CHECK(ASRUtils::trait_method_mismatch(*a, *b).difference ==
            ASRUtils::TraitMethodDifference::None);
        auto *type = ASR::down_cast<ASR::Array_t>(ASRUtils::expr_type(b->m_return_var));
        type->m_dims[0].m_length = ASRUtils::EXPR(ASR::make_IntegerConstant_t(
            al, type->base.base.loc, 2,
            ASRUtils::TYPE(ASR::make_Integer_t(al, type->base.base.loc, 4))));
        auto mismatch = ASRUtils::trait_method_mismatch(*a, *b);
        CHECK(mismatch.difference == ASRUtils::TraitMethodDifference::Contract);
        CHECK(mismatch.message == "result array shapes differ");
    }

    SUBCASE("an unresolved parent is allowed only by partial verification") {
        auto *parent = ASR::down_cast<ASR::ExternalSymbol_t>(
            module->m_symtab->get_symbol("root"));
        parent->m_external = nullptr;
        LCompilers::diag::Diagnostics partial;
        CHECK(LCompilers::asr_verify(*result.result, false, partial));
        LCompilers::diag::Diagnostics complete;
        CHECK_FALSE(LCompilers::asr_verify(*result.result, true, complete));
    }

    SUBCASE("a foreign raw parent cannot replace a visible import") {
        child->m_parents[2] = &other->base;
        LCompilers::diag::Diagnostics invalid;
        CHECK_FALSE(LCompilers::asr_verify(*result.result, true, invalid));
        REQUIRE(!invalid.diagnostics.empty());
        CHECK(invalid.diagnostics.back().code == "asr.verify.trait.parent_in_scope");
    }

    SUBCASE("inherited constraint membership uses identity rather than name") {
        child_constraint->m_requirements[0].m_member =
            unrelated->m_symtab->get_symbol("get_value");
        LCompilers::diag::Diagnostics invalid;
        CHECK_FALSE(LCompilers::asr_verify(*result.result, true, invalid));
        REQUIRE(!invalid.diagnostics.empty());
        CHECK(invalid.diagnostics.back().code ==
            "asr.verify.trait_constraint.member_belongs_to_trait");
    }

    SUBCASE("inherited conformance membership uses identity rather than name") {
        implementation->m_bindings[0].m_member =
            unrelated->m_symtab->get_symbol("get_value");
        LCompilers::diag::Diagnostics invalid;
        CHECK_FALSE(LCompilers::asr_verify(*result.result, true, invalid));
        REQUIRE(!invalid.diagnostics.empty());
        CHECK(invalid.diagnostics.back().code ==
            "asr.verify.trait_implementation.member_belongs_to_trait");
    }

    SUBCASE("distinct original requirements must not be dropped") {
        child_constraint->n_requirements--;
        LCompilers::diag::Diagnostics invalid;
        CHECK_FALSE(LCompilers::asr_verify(*result.result, true, invalid));
        REQUIRE(!invalid.diagnostics.empty());
        CHECK(invalid.diagnostics.back().code ==
            "asr.verify.trait_constraint.requirements_complete");
    }

    SUBCASE("normalized callables cannot be shared across parameters") {
        base_constraint->m_requirements[0].m_procedure =
            independent_constraint->m_requirements[0].m_procedure;
        LCompilers::diag::Diagnostics invalid;
        CHECK_FALSE(LCompilers::asr_verify(*result.result, true, invalid));
        REQUIRE(!invalid.diagnostics.empty());
        CHECK(invalid.diagnostics.back().code ==
            "asr.verify.trait_constraint.receiver_matches_parameter");
    }

    SUBCASE("coalesced conformance witnesses include the receiver binding") {
        auto *binding = const_cast<ASR::trait_binding_t*>(ASRUtils::find_trait_binding(
            *implementation, other->m_symtab->get_symbol("get_value")));
        REQUIRE(binding != nullptr);
        binding->m_self_argument = LCompilers::s2c(al, "step");
        LCompilers::diag::Diagnostics invalid;
        CHECK_FALSE(LCompilers::asr_verify(*result.result, true, invalid));
        REQUIRE(!invalid.diagnostics.empty());
        CHECK(invalid.diagnostics.back().code ==
            "asr.verify.trait_implementation.coalesced_binding_agrees");
    }
}

TEST_CASE("Trait array shape contracts distinguish assumed-size") {
    namespace ASR = LCompilers::ASR;
    namespace ASRUtils = LCompilers::ASRUtils;
    const std::string source = R"(
module trait_array_contracts_m
implicit none
abstract interface :: ILeft
    function count(a) result(r)
        integer, intent(in) :: a(:)
        integer :: r
    end function
    function fixed(a) result(r)
        integer, intent(in) :: a(3)
        integer :: r
    end function
end interface
abstract interface :: IRight
    function count(a) result(r)
        integer, intent(in) :: a(:)
        integer :: r
    end function
    function fixed(a) result(r)
        integer, intent(in) :: a(3)
        integer :: r
    end function
end interface
abstract interface, extends(ILeft + IRight) :: IChild
end interface
contains
function unused{ILeft + IRight :: T}(x) result(r)
    type(T), intent(in) :: x
    integer :: r
    r = 0
end function
end module
)";
    Allocator al(1024 * 1024);
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::CompilerOptions options;
    auto parsed = LCompilers::LFortran::parse(al, source, diagnostics, options);
    REQUIRE(parsed.ok);
    LCompilers::LocationManager lm;
    auto result = LCompilers::LFortran::ast_to_asr(
        al, *parsed.result, diagnostics, nullptr, false, options, lm);
    REQUIRE(result.ok);
    CHECK(LCompilers::asr_verify(*result.result, true, diagnostics));
    auto *module = ASR::down_cast<ASR::Module_t>(
        result.result->m_symtab->get_symbol("trait_array_contracts_m"));
    auto *left = ASR::down_cast<ASR::Trait_t>(module->m_symtab->get_symbol("ileft"));
    auto *right = ASR::down_cast<ASR::Trait_t>(module->m_symtab->get_symbol("iright"));
    auto *a = ASR::down_cast<ASR::Function_t>(left->m_symtab->get_symbol("count"));
    auto *b = ASR::down_cast<ASR::Function_t>(right->m_symtab->get_symbol("count"));
    auto *generic = ASR::down_cast<ASR::Template_t>(
        module->m_symtab->get_symbol("unused"));
    ASR::Function_t *normalized = nullptr;
    for (const auto &entry : generic->m_symtab->get_scope()) {
        if (!ASR::is_a<ASR::TraitConstraint_t>(*entry.second)) continue;
        auto *constraint = ASR::down_cast<ASR::TraitConstraint_t>(entry.second);
        for (size_t i = 0; i < constraint->n_requirements; i++) {
            const auto &requirement = constraint->m_requirements[i];
            if (ASRUtils::symbol_get_past_external(requirement.m_member) == &a->base) {
                normalized = ASR::down_cast<ASR::Function_t>(requirement.m_procedure);
            }
        }
    }
    REQUIRE(normalized != nullptr);
    auto set_representation = [](ASR::Function_t *procedure, size_t argument,
            ASR::array_physical_typeType physical_type) {
        for (auto *type : {ASRUtils::expr_type(procedure->m_args[argument]),
                ASRUtils::get_FunctionType(procedure)->m_arg_types[argument]}) {
            ASR::down_cast<ASR::Array_t>(type)->m_physical_type = physical_type;
        }
    };

    SUBCASE("inherited shapes must match in either order") {
        set_representation(b, 0, ASR::array_physical_typeType::UnboundedPointerArray);
        for (auto mismatch : {ASRUtils::trait_method_mismatch(*a, *b),
                ASRUtils::trait_method_mismatch(*b, *a)}) {
            CHECK(mismatch.difference == ASRUtils::TraitMethodDifference::Contract);
            CHECK(mismatch.message == "argument 'a' has different array shapes");
        }
        CHECK_FALSE(ASRUtils::trait_types_equal(a->m_args[0], b->m_args[0]));
        LCompilers::diag::Diagnostics invalid;
        CHECK_FALSE(LCompilers::asr_verify(*result.result, true, invalid));
        REQUIRE(!invalid.diagnostics.empty());
        CHECK(invalid.diagnostics.back().code ==
            "asr.verify.trait.inherited_signature_matches");
    }

    SUBCASE("normalization must preserve the member shape category") {
        set_representation(normalized, 1, ASR::array_physical_typeType::UnboundedPointerArray);
        LCompilers::diag::Diagnostics invalid;
        CHECK_FALSE(LCompilers::asr_verify(*result.result, true, invalid));
        REQUIRE(!invalid.diagnostics.empty());
        CHECK(invalid.diagnostics.back().code ==
            "asr.verify.trait_requirement.signature_matches");
    }

    SUBCASE("matching assumed-size contracts remain equivalent") {
        set_representation(a, 0, ASR::array_physical_typeType::UnboundedPointerArray);
        set_representation(b, 0, ASR::array_physical_typeType::UnboundedPointerArray);
        set_representation(normalized, 1, ASR::array_physical_typeType::UnboundedPointerArray);
        CHECK(ASRUtils::trait_method_mismatch(*a, *b).difference ==
            ASRUtils::TraitMethodDifference::None);
        CHECK(ASRUtils::trait_types_equal(a->m_args[0], b->m_args[0]));
        CHECK(LCompilers::asr_verify(*result.result, true, diagnostics));
    }

    SUBCASE("equivalent explicit-shape contracts can use different storage") {
        auto *a_fixed = ASR::down_cast<ASR::Function_t>(
            left->m_symtab->get_symbol("fixed"));
        auto *b_fixed = ASR::down_cast<ASR::Function_t>(
            right->m_symtab->get_symbol("fixed"));
        set_representation(a_fixed, 0, ASR::array_physical_typeType::DescriptorArray);
        set_representation(b_fixed, 0, ASR::array_physical_typeType::PointerArray);
        CHECK(ASRUtils::trait_method_mismatch(*a_fixed, *b_fixed).difference ==
            ASRUtils::TraitMethodDifference::None);
        CHECK(ASRUtils::trait_types_equal(a_fixed->m_args[0], b_fixed->m_args[0]));
        CHECK(LCompilers::asr_verify(*result.result, true, diagnostics));
    }
}

static std::string trait_designator_source(const std::string &left,
        const std::string &right, const std::string &declarations = "",
        bool normalize = true) {
    return "module trait_designators_m\nimplicit none\n" + declarations +
        "abstract interface :: ILeft\n" + left + "end interface\n"
        "abstract interface :: IRight\n" + right + "end interface\n"
        "abstract interface, extends(ILeft + IRight) :: IChild\nend interface\n" +
        (normalize ?
        "contains\nfunction unused{ILeft + IRight :: T}(x) result(r)\n"
        "type(T), intent(in) :: x\ninteger :: r\nr = 0\nend function\n" : "") +
        "end module\n";
}

TEST_CASE("Trait declaration copying is independent of dummy spelling") {
    namespace ASR = LCompilers::ASR;
    namespace ASRUtils = LCompilers::ASRUtils;
    for (const std::string name : {"a", "binput", "zinput"}) {
        for (bool scalar : {false, true}) {
            CAPTURE(name);
            CAPTURE(scalar);
            const std::string bound = scalar ? "n" : "n(1)";
            const std::string source =
                "module review_arrayitem_normalized_m\nimplicit none\n"
                "abstract interface :: IA\nfunction count(n, " + name + ") result(r)\n"
                "integer, intent(in) :: " + bound + "\n"
                "integer, intent(in) :: " + name + "(" + bound + ")\n"
                "integer :: r\nend function\nend interface\ncontains\n"
                "function unused{IA :: T}(x) result(r)\n"
                "type(T), intent(in) :: x\ninteger :: r\nr = 0\n"
                "end function\nend module\nprogram review_arrayitem_normalized\n"
                "use review_arrayitem_normalized_m\nimplicit none\nend program\n";
            ast_ser(source);
            asr_ser(source);
            asr_mod(source, "review_arrayitem_normalized_m");

            Allocator al(1024 * 1024);
            LCompilers::diag::Diagnostics diagnostics;
            LCompilers::CompilerOptions options;
            auto parsed = LCompilers::LFortran::parse(al, source, diagnostics, options);
            REQUIRE(parsed.ok);
            LCompilers::LocationManager lm;
            auto result = LCompilers::LFortran::ast_to_asr(
                al, *parsed.result, diagnostics, nullptr, false, options, lm);
            REQUIRE(result.ok);
            auto *module = ASR::down_cast<ASR::Module_t>(
                result.result->m_symtab->get_symbol("review_arrayitem_normalized_m"));
            auto *trait = ASR::down_cast<ASR::Trait_t>(module->m_symtab->get_symbol("ia"));
            auto *original = ASR::down_cast<ASR::Function_t>(
                trait->m_symtab->get_symbol("count"));
            auto *generic = ASR::down_cast<ASR::Template_t>(
                module->m_symtab->get_symbol("unused"));
            ASR::Function_t *normalized = nullptr;
            for (const auto &entry : generic->m_symtab->get_scope()) {
                if (!ASR::is_a<ASR::TraitConstraint_t>(*entry.second)) continue;
                auto *constraint = ASR::down_cast<ASR::TraitConstraint_t>(entry.second);
                REQUIRE(constraint->n_requirements == 1);
                normalized = ASR::down_cast<ASR::Function_t>(
                    constraint->m_requirements[0].m_procedure);
            }
            REQUIRE(normalized != nullptr);
            for (auto entry : {std::make_pair(original, size_t(0)),
                    std::make_pair(normalized, size_t(1))}) {
                auto *procedure = entry.first;
                size_t offset = entry.second;
                auto get_bound = [&](ASR::ttype_t *type) {
                    auto *length = ASR::down_cast<ASR::Array_t>(type)->m_dims[0].m_length;
                    return scalar ? length : ASR::down_cast<ASR::ArrayItem_t>(length)->m_v;
                };
                auto *declaration = get_bound(ASRUtils::expr_type(procedure->m_args[1 + offset]));
                CHECK(ASR::down_cast<ASR::Var_t>(declaration)->m_v ==
                    ASR::down_cast<ASR::Var_t>(procedure->m_args[offset])->m_v);
                auto *parameter = get_bound(
                    ASRUtils::get_FunctionType(procedure)->m_arg_types[1 + offset]);
                REQUIRE(ASR::is_a<ASR::FunctionParam_t>(*parameter));
                CHECK(ASR::down_cast<ASR::FunctionParam_t>(parameter)->m_param_number == offset);
            }
            CHECK(ASRUtils::EXPR2VAR(original->m_args[0]) !=
                ASRUtils::EXPR2VAR(normalized->m_args[1]));
        }
    }
}

TEST_CASE("Symbol copying remaps complete declarations without capturing host symbols") {
    namespace ASR = LCompilers::ASR;
    namespace ASRUtils = LCompilers::ASRUtils;
    const std::string source = R"(
module copy_declarations_m
implicit none
integer :: host_n
type :: Bounds
    integer :: n
end type
contains
function original(n, a, btext, box) result(r)
    integer, intent(in) :: n(1), a(n(1))
    character(len=n(1)), intent(in) :: btext
    type(Bounds), intent(in) :: box
    integer :: r(n(1)), member(box%n), host_bound(host_n)
    integer, parameter :: zseed = 3, aseed = zseed + 1
    r = a
    block
        integer :: early(n(1))
        early = a
    end block
end function
end module
)";
    Allocator al(1024 * 1024);
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::CompilerOptions options;
    auto parsed = LCompilers::LFortran::parse(al, source, diagnostics, options);
    REQUIRE(parsed.ok);
    LCompilers::LocationManager lm;
    auto result = LCompilers::LFortran::ast_to_asr(
        al, *parsed.result, diagnostics, nullptr, false, options, lm);
    REQUIRE(result.ok);
    auto *module = ASR::down_cast<ASR::Module_t>(
        result.result->m_symtab->get_symbol("copy_declarations_m"));
    auto *original = ASR::down_cast<ASR::Function_t>(
        module->m_symtab->get_symbol("original"));
    auto *original_a = ASRUtils::EXPR2VAR(original->m_args[1]);
    original_a->m_codims = al.allocate<ASR::codimension_t>(1);
    original_a->n_codims = 1;
    original_a->m_codims[0].loc = original_a->base.base.loc;
    original_a->m_codims[0].m_start =
        ASR::down_cast<ASR::Array_t>(original_a->m_type)->m_dims[0].m_length;
    original_a->m_codims[0].m_end = nullptr;
    original_a->m_codims[0].m_end_star = ASR::codimension_typeType::CodimensionStar;
    ASRUtils::SymbolDuplicator duplicator(al);
    auto *copy = ASR::down_cast<ASR::Function_t>(
        duplicator.duplicate_Function(original, module->m_symtab));
    copy->m_name = LCompilers::s2c(al, "copied");
    module->m_symtab->add_symbol("copied", &copy->base);
    auto variable = [](LCompilers::SymbolTable *scope, const std::string &name) {
        return ASR::down_cast<ASR::Variable_t>(scope->get_symbol(name));
    };
    auto length = [&](LCompilers::SymbolTable *scope, const std::string &name) {
        return ASR::down_cast<ASR::Array_t>(variable(scope, name)->m_type)->m_dims[0].m_length;
    };
    for (auto *procedure : {original, copy}) {
        auto *scope = procedure->m_symtab;
        auto *n = scope->get_symbol("n");
        for (const std::string name : {"a", "r"}) {
            auto *bound = ASR::down_cast<ASR::ArrayItem_t>(length(scope, name));
            CHECK(ASR::down_cast<ASR::Var_t>(bound->m_v)->m_v == n);
        }
        auto *codim = ASR::down_cast<ASR::ArrayItem_t>(
            variable(scope, "a")->m_codims[0].m_start);
        CHECK(ASR::down_cast<ASR::Var_t>(codim->m_v)->m_v == n);
        auto *text = ASR::down_cast<ASR::String_t>(variable(scope, "btext")->m_type);
        CHECK(ASR::down_cast<ASR::Var_t>(
            ASR::down_cast<ASR::ArrayItem_t>(text->m_len)->m_v)->m_v == n);
        auto *initializer = ASR::down_cast<ASR::IntegerBinOp_t>(
            variable(scope, "aseed")->m_symbolic_value);
        CHECK(ASR::down_cast<ASR::Var_t>(initializer->m_left)->m_v ==
            scope->get_symbol("zseed"));
        auto *member = ASR::down_cast<ASR::StructInstanceMember_t>(length(scope, "member"));
        CHECK(ASR::down_cast<ASR::Var_t>(member->m_v)->m_v == scope->get_symbol("box"));
        CHECK(ASRUtils::symbol_get_past_external(member->m_m) ==
            ASR::down_cast<ASR::Struct_t>(module->m_symtab->get_symbol("bounds"))->
                m_symtab->get_symbol("n"));
        CHECK(ASRUtils::symbol_get_past_external(variable(scope, "box")->m_type_declaration) ==
            module->m_symtab->get_symbol("bounds"));
        CHECK(ASR::down_cast<ASR::Var_t>(length(scope, "host_bound"))->m_v ==
            module->m_symtab->get_symbol("host_n"));
        size_t blocks = 0;
        for (const auto &entry : scope->get_scope()) {
            if (!ASR::is_a<ASR::Block_t>(*entry.second)) continue;
            blocks++;
            auto *block = ASR::down_cast<ASR::Block_t>(entry.second);
            auto *bound = ASR::down_cast<ASR::ArrayItem_t>(length(block->m_symtab, "early"));
            CHECK(ASR::down_cast<ASR::Var_t>(bound->m_v)->m_v == n);
        }
        CHECK(blocks == 1);
    }
    CHECK(copy->m_symtab->get_symbol("n") != original->m_symtab->get_symbol("n"));
    CHECK(LCompilers::asr_verify(*result.result, true, diagnostics));
    LCompilers::SymbolTable symtab(nullptr);
    auto *roundtrip = ASR::down_cast2<ASR::TranslationUnit_t>(
        LCompilers::deserialize_asr(
            al, LCompilers::serialize(*result.result), true, symtab, 0));
    fix_external_symbols(*roundtrip, symtab);
    CHECK(LCompilers::asr_verify(*roundtrip, true, diagnostics));

    auto *destination = al.make_new<LCompilers::SymbolTable>(module->m_symtab);
    duplicator.duplicate_symbol(module->m_symtab->get_symbol("host_n"), destination);
    duplicator.duplicate_SymbolTable(original->m_symtab, destination);
    CHECK(ASR::down_cast<ASR::Var_t>(length(destination, "host_bound"))->m_v ==
        module->m_symtab->get_symbol("host_n"));
    CHECK(destination->get_symbol("host_n") != module->m_symtab->get_symbol("host_n"));
}

TEST_CASE("Trait array-element bounds preserve positional correspondence") {
    namespace ASR = LCompilers::ASR;
    namespace ASRUtils = LCompilers::ASRUtils;
    const std::string method = R"(
function count(n, other, k, a) result(r)
    integer, intent(in) :: n(2, 2), other(2, 2), k(2)
    integer, intent(in) :: a(n(k(1), 2), other(1, 1))
    integer :: r
end function
)";
    const std::string source = trait_designator_source(method, method);
    Allocator al(1024 * 1024);
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::CompilerOptions options;
    auto parsed = LCompilers::LFortran::parse(al, source, diagnostics, options);
    REQUIRE(parsed.ok);
    LCompilers::LocationManager lm;
    auto result = LCompilers::LFortran::ast_to_asr(
        al, *parsed.result, diagnostics, nullptr, false, options, lm);
    REQUIRE(result.ok);
    CHECK(LCompilers::asr_verify(*result.result, true, diagnostics));
    auto *module = ASR::down_cast<ASR::Module_t>(
        result.result->m_symtab->get_symbol("trait_designators_m"));
    auto member = [&](const std::string &name) {
        auto *trait = ASR::down_cast<ASR::Trait_t>(module->m_symtab->get_symbol(name));
        return ASR::down_cast<ASR::Function_t>(trait->m_symtab->get_symbol("count"));
    };
    auto *a = member("ileft");
    auto *b = member("iright");
    auto *generic = ASR::down_cast<ASR::Template_t>(
        module->m_symtab->get_symbol("unused"));
    ASR::Function_t *normalized = nullptr;
    size_t requirements = 0;
    for (const auto &entry : generic->m_symtab->get_scope()) {
        if (!ASR::is_a<ASR::TraitConstraint_t>(*entry.second)) continue;
        auto *constraint = ASR::down_cast<ASR::TraitConstraint_t>(entry.second);
        REQUIRE(constraint->n_requirements == 1);
        auto *procedure = ASR::down_cast<ASR::Function_t>(
            constraint->m_requirements[0].m_procedure);
        if (normalized) CHECK(normalized == procedure);
        normalized = procedure;
        requirements++;
    }
    REQUIRE(requirements == 2);
    REQUIRE(normalized != nullptr);
    CHECK(ASRUtils::trait_method_mismatch(*a, *normalized, 0, 1).difference ==
        ASRUtils::TraitMethodDifference::None);
    CHECK(ASRUtils::trait_method_mismatch(*normalized, *b, 1, 0).difference ==
        ASRUtils::TraitMethodDifference::None);

    auto item = [](ASR::ttype_t *type, size_t dimension) {
        return ASR::down_cast<ASR::ArrayItem_t>(
            ASR::down_cast<ASR::Array_t>(type)->m_dims[dimension].m_length);
    };
    for (auto entry : {std::make_pair(a, size_t(0)),
            std::make_pair(b, size_t(0)), std::make_pair(normalized, size_t(1))}) {
        auto *procedure = entry.first;
        size_t offset = entry.second;
        auto *signature = ASRUtils::get_FunctionType(procedure);
        REQUIRE(signature->n_arg_types == 4 + offset);
        auto *type = signature->m_arg_types[3 + offset];
        auto *first = item(type, 0);
        auto *index = ASR::down_cast<ASR::ArrayItem_t>(first->m_args[0].m_right);
        for (auto parameter : {
                std::make_pair(first->m_v, offset),
                std::make_pair(index->m_v, offset + 2),
                std::make_pair(item(type, 1)->m_v, offset + 1)}) {
            auto *expr = parameter.first;
            size_t position = parameter.second;
            REQUIRE(ASR::is_a<ASR::FunctionParam_t>(*expr));
            CHECK(ASR::down_cast<ASR::FunctionParam_t>(expr)->m_param_number ==
                position);
        }
    }
    auto verify_rejection = [&](const std::string &code) {
        LCompilers::PassUtils::UpdateDependenciesVisitor dependencies(al);
        dependencies.visit_TranslationUnit(*result.result);
        auto check = [&](ASR::TranslationUnit_t *unit) {
            LCompilers::diag::Diagnostics invalid;
            CHECK_FALSE(LCompilers::asr_verify(*unit, true, invalid));
            REQUIRE(!invalid.diagnostics.empty());
            CHECK(invalid.diagnostics.back().code == code);
        };
        check(result.result);
        LCompilers::SymbolTable symtab(nullptr);
        auto *copy = ASR::down_cast2<ASR::TranslationUnit_t>(
            LCompilers::deserialize_asr(
                al, LCompilers::serialize(*result.result), true, symtab, 0));
        fix_external_symbols(*copy, symtab);
        check(copy);
    };
    auto integer = [&](ASR::expr_t *old, int value) {
        return ASRUtils::EXPR(ASR::make_IntegerConstant_t(
            al, old->base.loc, value, ASRUtils::expr_type(old)));
    };

    SUBCASE("faithful contracts and normalized copies serialize and verify") {
        ast_ser(source);
        asr_ser(source);
        asr_mod(source);
    }
    SUBCASE("inherited contracts compare every subscript in either order") {
        for (auto *type : {ASRUtils::expr_type(b->m_args[3]),
                ASRUtils::get_FunctionType(b)->m_arg_types[3]}) {
            auto &subscript = item(type, 0)->m_args[1].m_right;
            subscript = integer(subscript, 1);
        }
        CHECK(ASRUtils::trait_method_mismatch(*a, *b).difference ==
            ASRUtils::TraitMethodDifference::Contract);
        CHECK(ASRUtils::trait_method_mismatch(*b, *a).difference ==
            ASRUtils::TraitMethodDifference::Contract);
        verify_rejection("asr.verify.trait.inherited_signature_matches");
    }
    SUBCASE("normalization preserves nested subscripts") {
        for (auto *type : {ASRUtils::expr_type(normalized->m_args[4]),
                ASRUtils::get_FunctionType(normalized)->m_arg_types[4]}) {
            auto *index = ASR::down_cast<ASR::ArrayItem_t>(
                item(type, 0)->m_args[0].m_right);
            auto &subscript = index->m_args[0].m_right;
            subscript = integer(subscript, 2);
        }
        verify_rejection("asr.verify.trait_requirement.signature_matches");
    }
    SUBCASE("equal-shaped ordinary dummies are not interchangeable") {
        item(ASRUtils::expr_type(b->m_args[3]), 0)->m_v = b->m_args[1];
        auto *base = item(ASRUtils::get_FunctionType(b)->m_arg_types[3], 0)->m_v;
        ASR::down_cast<ASR::FunctionParam_t>(base)->m_param_number = 1;
        verify_rejection("asr.verify.trait.inherited_signature_matches");
    }
    SUBCASE("unsupported expressions are not equal even when shared") {
        auto *unknown = ASRUtils::EXPR(ASR::make_ArrayRank_t(
            al, a->base.base.loc, a->m_args[0],
            ASRUtils::expr_type(a->m_return_var), nullptr));
        for (auto *procedure : {a, b}) {
            ASR::down_cast<ASR::Array_t>(
                ASRUtils::expr_type(procedure->m_args[3]))->m_dims[0].m_length = unknown;
        }
        CHECK(ASRUtils::trait_method_mismatch(*a, *b).difference ==
            ASRUtils::TraitMethodDifference::Contract);
    }
}

TEST_CASE("Trait specification designators compare bases and selectors") {
    std::string arguments, declarations, bound, different, types;
    SUBCASE("components retain original member identity") {
        types = "type Bounds\ninteger :: extent(2), other(2)\nend type\n";
        arguments = "n";
        declarations = "import :: Bounds\ntype(Bounds), intent(in) :: n\n";
        bound = "n%extent(1)";
        different = "n%other(1)";
    }
    SUBCASE("substrings retain both limits") {
        arguments = "text, first, last";
        declarations = "character(*), intent(in) :: text\n"
            "integer, intent(in) :: first, last\n";
        bound = "len(text(first:last))";
        different = "len(text(last:first))";
    }
    SUBCASE("character items retain their subscript") {
        arguments = "text, first, last";
        declarations = "character(*), intent(in) :: text\n"
            "integer, intent(in) :: first, last\n";
        bound = "iachar(text(first:first))";
        different = "iachar(text(last:last))";
    }
    SUBCASE("array sections retain their stride") {
        arguments = "n, last";
        types = "interface\npure integer function extent(n) result(r)\n"
            "integer, intent(in) :: n(:)\nend function\nend interface\n";
        declarations = "import :: extent\ninteger, intent(in) :: n(:), last\n";
        bound = "extent(n(1:last:2))";
        different = "extent(n(1:last:1))";
    }
    auto method = [&](const std::string &length) {
        return "function count(" + arguments + ", a) result(r)\n" +
            declarations + "integer, intent(in) :: a(" + length + ")\n"
            "integer :: r\nend function\n";
    };
    const std::string source = trait_designator_source(
        method(bound), method(bound), types, false);
    asr_ser(source);
    asr_mod(source);

    Allocator al(1024 * 1024);
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::CompilerOptions options;
    const std::string bad_source = trait_designator_source(
        method(bound), method(different), types, false);
    auto parsed = LCompilers::LFortran::parse(al, bad_source, diagnostics, options);
    REQUIRE(parsed.ok);
    LCompilers::LocationManager lm;
    auto result = LCompilers::LFortran::ast_to_asr(
        al, *parsed.result, diagnostics, nullptr, false, options, lm);
    CHECK_FALSE(result.ok);
    REQUIRE(diagnostics.has_error());
    CHECK(diagnostics.diagnostics.back().stage == LCompilers::diag::Stage::Semantic);
    CHECK(diagnostics.diagnostics.back().message.find("different array shapes") !=
        std::string::npos);

    Allocator recovery_al(1024 * 1024);
    LCompilers::diag::Diagnostics recovery_diagnostics;
    options.continue_compilation = true;
    const std::string recovery_source = bad_source +
        "module after_trait_bound_failure\ninteger :: marker = 17\nend module\n";
    auto recovery_ast = LCompilers::LFortran::parse(
        recovery_al, recovery_source, recovery_diagnostics, options);
    REQUIRE(recovery_ast.ok);
    auto recovered = LCompilers::LFortran::ast_to_asr(
        recovery_al, *recovery_ast.result, recovery_diagnostics,
        nullptr, false, options, lm);
    REQUIRE(recovered.ok);
    CHECK(recovery_diagnostics.has_error());
    CHECK(recovered.result->m_symtab->get_symbol("after_trait_bound_failure") != nullptr);
    LCompilers::diag::Diagnostics verification;
    CHECK(LCompilers::asr_verify(*recovered.result, true, verification));
}

TEST_CASE("Recursive trait forwarding preserves canonical backedges") {
    namespace ASR = LCompilers::ASR;
    namespace ASRUtils = LCompilers::ASRUtils;
    const std::string source = R"(
module recursive_traits_m
implicit none
abstract interface :: IValue
    function value() result(r)
        integer :: r
    end function
end interface
type :: Box
    integer :: data
end type
implements IValue :: Box
    procedure, pass :: value => box_value
end implements
contains
function box_value(self) result(r)
    class(Box), intent(in) :: self
    integer :: r
    r = self%data
end function
recursive function first{IValue :: T}(x, n) result(r)
    type(T), intent(in) :: x
    integer, intent(in) :: n
    integer :: r
    if (n == 0) then
        r = x%value()
    else
        r = second(x, n-1) + 1
    end if
end function
recursive function second{IValue :: U}(x, n) result(r)
    type(U), intent(in) :: x
    integer, intent(in) :: n
    integer :: r
    if (n == 0) then
        r = x%value()
    else
        r = first{U}(x, n-1) + 1
    end if
end function
end module
program recursive_traits
use recursive_traits_m
implicit none
type(Box) :: x
integer :: a, b
x = Box(11)
a = first(x, 4)
b = second(x, 4)
end program
)";
    asr_ser(source);
    asr_mod(source, "recursive_traits_m");

    Allocator al(1024 * 1024);
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::CompilerOptions options;
    auto parsed = LCompilers::LFortran::parse(al, source, diagnostics, options);
    REQUIRE(parsed.ok);
    LCompilers::LocationManager lm;
    auto result = LCompilers::LFortran::ast_to_asr(
        al, *parsed.result, diagnostics, nullptr, false, options, lm);
    REQUIRE(result.ok);

    auto recursive_callee = [](ASR::Function_t *function) {
        // Both source functions have a base case and a recursive branch.
        REQUIRE(function->n_body == 1);
        auto *branch = ASR::down_cast<ASR::If_t>(function->m_body[0]);
        REQUIRE(branch->n_body == 1);
        REQUIRE(branch->n_orelse == 1);
        auto *assignment = ASR::down_cast<ASR::Assignment_t>(branch->m_orelse[0]);
        auto *addition = ASR::down_cast<ASR::IntegerBinOp_t>(assignment->m_value);
        auto *call = ASR::down_cast<ASR::FunctionCall_t>(addition->m_left);
        return ASR::down_cast<ASR::Function_t>(
            ASRUtils::symbol_get_past_external(call->m_name));
    };
    auto check_cycles = [&](ASR::TranslationUnit_t *unit) {
        CHECK(LCompilers::asr_verify(*unit, true, diagnostics));
        auto *program = ASR::down_cast<ASR::Program_t>(
            unit->m_symtab->get_symbol("recursive_traits"));
        REQUIRE(program->n_body == 3);
        auto entry = [&](size_t i) {
            auto *assignment = ASR::down_cast<ASR::Assignment_t>(program->m_body[i]);
            auto *call = ASR::down_cast<ASR::FunctionCall_t>(assignment->m_value);
            return ASR::down_cast<ASR::Function_t>(
                ASRUtils::symbol_get_past_external(call->m_name));
        };
        auto *first = entry(1);
        auto *second = entry(2);
        CHECK(first != second);
        CHECK(recursive_callee(first) == second);
        CHECK(recursive_callee(second) == first);

        auto *module = ASR::down_cast<ASR::Module_t>(
            unit->m_symtab->get_symbol("recursive_traits_m"));
        for (const char *name : {"first", "second"}) {
            auto *generic = ASR::down_cast<ASR::Template_t>(
                module->m_symtab->get_symbol(name));
            auto *original = ASR::down_cast<ASR::Function_t>(
                generic->m_symtab->get_symbol(name));
            auto *forwarded = recursive_callee(original);
            CHECK(forwarded != original);
            CHECK(recursive_callee(forwarded) == original);
        }
    };
    check_cycles(result.result);
    LCompilers::SymbolTable symtab(nullptr);
    auto *copy = ASR::down_cast2<ASR::TranslationUnit_t>(
        LCompilers::deserialize_asr(
            al, LCompilers::serialize(*result.result), true, symtab, 0));
    fix_external_symbols(*copy, symtab);
    check_cycles(copy);
}

TEST_CASE("Trait forwarding checks nominal implications before instantiation") {
    const std::string declarations = R"(
module forwarding_contracts
implicit none
abstract interface :: IBase
    function value() result(r)
        integer :: r
    end function
end interface
abstract interface, extends(IBase) :: IChild
end interface
abstract interface :: IOther
    function value() result(r)
        integer :: r
    end function
end interface
contains
)";
    std::string source;
    SUBCASE("parent constraints do not imply the child") {
        source = declarations + R"(
function required{IChild :: T}(object) result(r)
    type(T), intent(in) :: object
    integer :: r
    r = object%value()
end function
function invalid{IBase :: T}(object) result(r)
    type(T), intent(in) :: object
    integer :: r
    r = required(object)
end function
end module
)";
    }
    SUBCASE("same-name methods do not imply an unrelated nominal trait") {
        source = declarations + R"(
function required{IBase + IOther :: U}(object) result(r)
    type(U), intent(in) :: object
    integer :: r
    r = object%value()
end function
function invalid{IChild :: T}(object) result(r)
    type(T), intent(in) :: object
    integer :: r
    r = required(object)
end function
end module
)";
    }
    SUBCASE("evidence cannot be borrowed from a different binder") {
        source = declarations + R"(
function required{IChild :: T}(object) result(r)
    type(T), intent(in) :: object
    integer :: r
    r = object%value()
end function
function invalid{IChild :: T, IBase :: U}(a, b) result(r)
    type(T), intent(in) :: a
    type(U), intent(in) :: b
    integer :: r
    r = required(b)
end function
end module
)";
    }
    REQUIRE(!source.empty());
    Allocator al(1024 * 1024);
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::CompilerOptions options;
    auto parsed = LCompilers::LFortran::parse(al, source, diagnostics, options);
    REQUIRE(parsed.ok);
    LCompilers::LocationManager lm;
    auto result = LCompilers::LFortran::ast_to_asr(
        al, *parsed.result, diagnostics, nullptr, false, options, lm);
    CHECK_FALSE(result.ok);
    REQUIRE(!diagnostics.diagnostics.empty());
    CHECK(diagnostics.diagnostics.back().message.find("do not imply trait") != std::string::npos);
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
