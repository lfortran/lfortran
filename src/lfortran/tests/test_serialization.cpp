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
#include <libasr/asr_verify.h>
#include <libasr/pass/pass_utils.h>
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

TEST_CASE("Trait array-element bounds preserve positional correspondence") {
    namespace ASR = LCompilers::ASR;
    namespace ASRUtils = LCompilers::ASRUtils;
    const std::string method = R"(
function count(n, other, k, zinput) result(r)
    integer, intent(in) :: n(2, 2), other(2, 2), k(2)
    integer, intent(in) :: zinput(n(k(1), 2), other(1, 1))
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
    auto parsed = LCompilers::LFortran::parse(al, trait_designator_source(
        method(bound), method(different), types, false), diagnostics, options);
    REQUIRE(parsed.ok);
    LCompilers::LocationManager lm;
    auto result = LCompilers::LFortran::ast_to_asr(
        al, *parsed.result, diagnostics, nullptr, false, options, lm);
    CHECK_FALSE(result.ok);
    REQUIRE(diagnostics.has_error());
    CHECK(diagnostics.diagnostics.back().message.find("different array shapes") !=
        std::string::npos);
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
