#include <tests/doctest.h>

#include <libasr/asr_text.h>
#include <libasr/asr_verify.h>
#include <lfortran/fortran_evaluator.h>

using LCompilers::ASRTextForm;
using LCompilers::ASRTextOptions;
using LCompilers::FortranEvaluator;
using LCompilers::Location;
using LCompilers::LocationManager;
using LCompilers::Result;
using LCompilers::diag::Diagnostics;

TEST_CASE("ASR text printer emits canonical named and positional forms") {
    const std::string source = R"(
program p
    integer :: x
    x = 1
end program
)";

    LCompilers::CompilerOptions compiler_options;
    FortranEvaluator evaluator(compiler_options);
    LocationManager lm;
    LocationManager::FileLocations file;
    file.in_filename = "printer_test.f90";
    lm.files.push_back(file);
    lm.file_ends.push_back(source.size());
    Diagnostics diagnostics;

    Result<LCompilers::ASR::TranslationUnit_t *> result =
        evaluator.get_asr2(source, lm, diagnostics);
    REQUIRE(result.ok);
    REQUIRE(LCompilers::asr_verify(*result.result, true, diagnostics));

    ASRTextOptions named_options;
    named_options.form = ASRTextForm::Named;
    named_options.indent = false;
    const std::string named =
        LCompilers::asr_to_text(*result.result, named_options);

    CHECK(named.rfind("#asr/v1 ", 0) == 0);
    CHECK(named.find("(TranslationUnit :symtab ") != std::string::npos);
    CHECK(named.find("(Var :v #asr/sym [") != std::string::npos);
    CHECK(named.find(":realloc_lhs false") != std::string::npos);
    CHECK(named.find("\033") == std::string::npos);
    CHECK(named.find("....") == std::string::npos);

    ASRTextOptions positional_options;
    positional_options.form = ASRTextForm::Positional;
    positional_options.indent = false;
    const std::string positional =
        LCompilers::asr_to_text(*result.result, positional_options);

    CHECK(positional.rfind("#asr/v1 ", 0) == 0);
    CHECK(positional.find("(TranslationUnit (SymbolTable ") !=
        std::string::npos);
    CHECK(positional.find("(Var #asr/sym [") != std::string::npos);
    CHECK(positional.find(":realloc_lhs") == std::string::npos);
    CHECK(LCompilers::asr_to_text(*result.result, named_options) == named);

    Allocator named_allocator(1024 * 1024);
    LocationManager named_lm;
    Diagnostics named_diagnostics;
    Result<LCompilers::ASR::TranslationUnit_t *> named_parsed =
        LCompilers::asr_from_text(named_allocator, named, "named.asr",
            named_lm, named_diagnostics);
    REQUIRE(named_parsed.ok);
    CHECK(LCompilers::asr_verify(
        *named_parsed.result, true, named_diagnostics));
    CHECK(LCompilers::asr_to_text(
        *named_parsed.result, named_options) == named);

    Allocator positional_allocator(1024 * 1024);
    LocationManager positional_lm;
    Diagnostics positional_diagnostics;
    Result<LCompilers::ASR::TranslationUnit_t *> positional_parsed =
        LCompilers::asr_from_text(positional_allocator, positional,
            "positional.asr", positional_lm, positional_diagnostics);
    INFO(positional_diagnostics.render2());
    REQUIRE(positional_parsed.ok);
    CHECK(LCompilers::asr_verify(
        *positional_parsed.result, true, positional_diagnostics));
    CHECK(LCompilers::asr_to_text(
        *positional_parsed.result, positional_options) == positional);

    std::string invalid = named;
    const std::string valid_realloc = ":realloc_lhs false";
    size_t realloc_pos = invalid.find(valid_realloc);
    REQUIRE(realloc_pos != std::string::npos);
    invalid.replace(realloc_pos, valid_realloc.size(),
        ":realloc_lhs true");

    Allocator invalid_allocator(1024 * 1024);
    LocationManager invalid_lm;
    Diagnostics invalid_diagnostics;
    Result<LCompilers::ASR::TranslationUnit_t *> invalid_parsed =
        LCompilers::asr_from_text(invalid_allocator, invalid,
            "invalid.asr", invalid_lm, invalid_diagnostics);
    REQUIRE(invalid_parsed.ok);
    CHECK_FALSE(LCompilers::asr_verify(
        *invalid_parsed.result, true, invalid_diagnostics));
    REQUIRE(!invalid_diagnostics.diagnostics.empty());
    const LCompilers::diag::Diagnostic &verify_error =
        invalid_diagnostics.diagnostics.back();
    CHECK(verify_error.stage == LCompilers::diag::Stage::ASRVerify);
    CHECK(verify_error.message ==
        "Reallocation of non allocatable variable is not allowed");
    REQUIRE(!verify_error.labels.empty());
    REQUIRE(!verify_error.labels[0].spans.empty());
    const Location verify_loc = verify_error.labels[0].spans[0].loc;
    CHECK(invalid.substr(verify_loc.first,
        verify_loc.last - verify_loc.first + 1) == "Assignment");
}

TEST_CASE("ASR standalone verification requires one main program") {
    const std::string source = R"(
subroutine f()
end subroutine
)";

    LCompilers::CompilerOptions compiler_options;
    FortranEvaluator evaluator(compiler_options);
    LocationManager lm;
    LocationManager::FileLocations file;
    file.in_filename = "standalone_test.f90";
    lm.files.push_back(file);
    lm.file_ends.push_back(source.size());
    Diagnostics diagnostics;

    Result<LCompilers::ASR::TranslationUnit_t *> result =
        evaluator.get_asr2(source, lm, diagnostics);
    REQUIRE(result.ok);

    LCompilers::ASRVerifyOptions verify_options;
    verify_options.require_main_program = true;
    CHECK_FALSE(LCompilers::asr_verify(
        *result.result, verify_options, diagnostics));
    REQUIRE(!diagnostics.diagnostics.empty());
    CHECK(diagnostics.diagnostics.back().stage ==
        LCompilers::diag::Stage::ASRVerify);
    CHECK(diagnostics.diagnostics.back().message ==
        "standalone ASR must contain exactly one main program");
}

TEST_CASE("ASR text supports explicit location overrides") {
    const std::string text =
        "#asr/v1 #asr/loc [[1 2] "
        "(TranslationUnit "
        ":symtab (SymbolTable :id :st0 :symbols {}) "
        ":items [])]";

    Allocator allocator(1024 * 1024);
    LocationManager lm;
    Diagnostics diagnostics;
    Result<LCompilers::ASR::TranslationUnit_t *> parsed =
        LCompilers::asr_from_text(
            allocator, text, "location.asr", lm, diagnostics);
    REQUIRE(parsed.ok);
    CHECK(parsed.result->base.base.loc.first == 1);
    CHECK(parsed.result->base.base.loc.last == 2);
}

TEST_CASE("ASR text reports schema errors separately from verification") {
    const std::string text =
        "#asr/v1 (TranslationUnit "
        ":symtab (SymbolTable :id :st0 :symbols {}))";

    Allocator allocator(1024 * 1024);
    LocationManager lm;
    Diagnostics diagnostics;
    Result<LCompilers::ASR::TranslationUnit_t *> parsed =
        LCompilers::asr_from_text(
            allocator, text, "schema_error.asr", lm, diagnostics);
    CHECK_FALSE(parsed.ok);
    REQUIRE(!diagnostics.diagnostics.empty());
    CHECK(diagnostics.diagnostics.back().stage ==
        LCompilers::diag::Stage::ASRParser);
    CHECK(diagnostics.diagnostics.back().message.find(
        "expects 2 positional fields or exactly 2 named fields") !=
        std::string::npos);
}
