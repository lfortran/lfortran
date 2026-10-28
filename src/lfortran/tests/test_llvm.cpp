#include <tests/doctest.h>

#include <cmath>
#include <cstring>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <fstream>

#include <lfortran/fortran_evaluator.h>
#include <lfortran/parser/source_offset.h>
#include <libasr/codegen/evaluator.h>
#include <libasr/exception.h>
#include <lfortran/ast.h>
#include <libasr/asr.h>
#include <lfortran/parser/parser.h>
#include <lfortran/semantics/ast_to_asr.h>
#include <libasr/codegen/asr_to_llvm.h>
#include <lfortran/pickle.h>
#include <libasr/pickle.h>
#include <libasr/modfile.h>
#include <libasr/utils.h>
#include <lfortran/utils.h>

#include <llvm/Config/llvm-config.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/Target/TargetMachine.h>
#if LLVM_VERSION_MAJOR >= 17
#include <llvm/TargetParser/Triple.h>
#else
#include <llvm/ADT/Triple.h>
#endif

using LCompilers::TRY;
using LCompilers::FortranEvaluator;
using LCompilers::CompilerOptions;

// Raw LLVMEvaluator tests use ORC JIT which is not available under Emscripten;
// FortranEvaluator tests below are WASM-compatible via WasmLFortranExecutor dispatch.
#ifndef __EMSCRIPTEN__

TEST_CASE("LLVM target configuration") {
    CompilerOptions default_options;
    LCompilers::LLVMTargetConfig default_config
        = LCompilers::resolve_llvm_target_config(default_options);
    CHECK(default_config.cpu == "generic");
    CHECK(!default_config.emit_cpu_attribute);
    CHECK(default_config.features.empty());

    CompilerOptions fast_options;
    fast_options.po.fast = true;
    LCompilers::LLVMTargetConfig fast_config
        = LCompilers::resolve_llvm_target_config(fast_options);
    CHECK(fast_config.host_target);
    CHECK(fast_config.emit_cpu_attribute);
    CHECK(!fast_config.cpu.empty());

    CompilerOptions generic_options;
    generic_options.po.fast = true;
    generic_options.mcpu = "generic";
    LCompilers::LLVMTargetConfig generic_config
        = LCompilers::resolve_llvm_target_config(generic_options);
    CHECK(generic_config.cpu == "generic");
    CHECK(generic_config.emit_cpu_attribute);
    CHECK(generic_config.features.empty());

    CompilerOptions tune_options;
    tune_options.mtune = "generic";
    LCompilers::LLVMTargetConfig tune_config
        = LCompilers::resolve_llvm_target_config(tune_options);
    CHECK(tune_config.cpu == "generic");
    CHECK(tune_config.tune_cpu == "generic");
    CHECK(!tune_config.emit_cpu_attribute);

    llvm::LLVMContext context;
    llvm::Module module("target_options", context);
    llvm::FunctionType *function_type = llvm::FunctionType::get(
        llvm::Type::getVoidTy(context), false);
    llvm::Function *function = llvm::Function::Create(function_type,
        llvm::Function::ExternalLinkage, "f", &module);
    llvm::BasicBlock *entry = llvm::BasicBlock::Create(
        context, "entry", function);
    llvm::ReturnInst::Create(context, entry);
    generic_config.apply_target_attributes(module);
    CHECK(function->getFnAttribute("target-cpu").getValueAsString()
        == "generic");

    llvm::Triple triple(default_config.triple);
    CompilerOptions march_options;
    CompilerOptions cpu_and_march_options;
    if (triple.isAArch64()) {
        // `armv8.7-a` is only known to LLVM 12 and later, and `apple-m1`
        // only to LLVM 13 and later, so pick names that every supported
        // LLVM version can parse. `armv8-a` is deliberately avoided here:
        // it is the AArch64 baseline, and LLVM releases before 12 report no
        // feature at all for it, so it cannot be used to check that `--march`
        // contributes features.
#if LLVM_VERSION_MAJOR >= 12
        march_options.march = "armv8.7-a";
#else
        march_options.march = "armv8.2-a";
#endif
        cpu_and_march_options.march = "armv8.1-a";
        cpu_and_march_options.mcpu = "cortex-a57";
    } else if (triple.getArch() == llvm::Triple::x86_64) {
        // `llvm/Support/X86TargetParser.h` only exists from LLVM 11 on, so
        // `--march` cannot be resolved for x86 against older LLVM releases
        // and there is nothing to check here.
#if LLVM_VERSION_MAJOR >= 11
        // The `x86-64-vN` micro-architecture levels were added in LLVM 12.
#if LLVM_VERSION_MAJOR >= 12
        march_options.march = "x86-64-v3";
#else
        march_options.march = "x86-64";
#endif
        cpu_and_march_options.march = "x86-64";
        cpu_and_march_options.mcpu = "generic";
#endif
    }
    if (!march_options.march.empty()) {
        LCompilers::LLVMTargetConfig march_config
            = LCompilers::resolve_llvm_target_config(march_options);
        CHECK(march_config.cpu == "generic");
        CHECK(!march_config.features.empty());

        LCompilers::LLVMTargetConfig cpu_and_march_config
            = LCompilers::resolve_llvm_target_config(
                cpu_and_march_options);
        CHECK(cpu_and_march_config.cpu == "generic");
        CHECK(cpu_and_march_config.tune_cpu
            == cpu_and_march_options.mcpu);
        CHECK(!cpu_and_march_config.features.empty());
    }

    // `native` must resolve on every host. LLVM reports the CPU as "generic"
    // when it cannot identify it (any CPU newer than the LLVM release in use,
    // e.g. Zen 3 under LLVM 11), and "generic" is not a name its architecture
    // tables accept, so this used to abort the compilation.
    CompilerOptions native_march_options;
    native_march_options.march = "native";
    CHECK_NOTHROW(
        LCompilers::resolve_llvm_target_config(native_march_options));

    CompilerOptions native_mcpu_options;
    native_mcpu_options.mcpu = "native";
    CHECK_NOTHROW(
        LCompilers::resolve_llvm_target_config(native_mcpu_options));

    CompilerOptions native_mtune_options;
    native_mtune_options.mtune = "native";
    CHECK_NOTHROW(
        LCompilers::resolve_llvm_target_config(native_mtune_options));

    CompilerOptions invalid_cpu_options;
    invalid_cpu_options.mcpu = "not-a-real-cpu";
    CHECK_THROWS_AS(
        LCompilers::resolve_llvm_target_config(invalid_cpu_options),
        LCompilers::LCompilersException);
}

TEST_CASE("LLVM code generation optimization level") {
    // Like clang and flang, the default mode generates machine code at -O0
    // and `--fast` at -O3.
#if LLVM_VERSION_MAJOR >= 18
    using CodeGenOptLevel = llvm::CodeGenOptLevel;
#else
    using CodeGenOptLevel = llvm::CodeGenOpt::Level;
#endif
    CompilerOptions default_options;
    LCompilers::LLVMEvaluator default_evaluator(default_options);
    CHECK(default_evaluator.get_target_machine().getOptLevel()
        == CodeGenOptLevel::None);

    CompilerOptions fast_options;
    fast_options.po.fast = true;
    LCompilers::LLVMEvaluator fast_evaluator(fast_options);
    CHECK(fast_evaluator.get_target_machine().getOptLevel()
        == CodeGenOptLevel::Aggressive);

#ifdef HAVE_TARGET_WASM
    // WebAssembly needs register coloring to keep functions within the
    // engines' limit on the number of locals.
    LCompilers::LLVMEvaluator wasm_evaluator("wasm32-unknown-wasi");
    CHECK(wasm_evaluator.get_target_machine().getOptLevel()
        == CodeGenOptLevel::Default);
#endif

#ifdef HAVE_TARGET_AARCH64
    // At -O0 LLVM selects GlobalISel on AArch64, whose compile time is
    // quadratic in the size of a basic block; the default mode uses FastISel
    // as on x86-64.
    for (const std::string triple: {"aarch64-unknown-linux-gnu",
            "arm64-apple-macosx11.0.0"}) {
        LCompilers::LLVMEvaluator aarch64_evaluator(triple);
        const llvm::TargetMachine &tm = aarch64_evaluator.get_target_machine();
        CHECK(tm.getOptLevel() == CodeGenOptLevel::None);
        CHECK(!tm.Options.EnableGlobalISel);
    }
#endif
}

TEST_CASE("llvm 1") {
    //std::cout << "LLVM Version:" << std::endl;
    //LFortran::LLVMEvaluator::print_version_message();

    LCompilers::LLVMEvaluator e;
    e.add_module(R"""(
define i64 @f1()
{
    ret i64 4
}
    )""");
    CHECK(e.execfn<int64_t>("f1") == 4);
    e.add_module("");
//    CHECK(e.execfn<int64_t>("f1") == 4);

/*
    e.add_module(R"""(
define i64 @f1()
{
    ret i64 5
}
    )""");
    CHECK(e.execfn<int64_t>("f1") == 5);
    e.add_module("");
    CHECK(e.execfn<int64_t>("f1") == 5);
*/
}

TEST_CASE("llvm 1 fail") {
    LCompilers::LLVMEvaluator e;
    CHECK_THROWS_AS(e.add_module(R"""(
define i64 @f1()
{
    ; FAIL: "=x" is incorrect syntax
    %1 =x alloca i64
}
        )"""), LCompilers::LCompilersException);
    CHECK_THROWS_WITH(e.add_module(R"""(
define i64 @f1()
{
    ; FAIL: "=x" is incorrect syntax
    %1 =x alloca i64
}
        )"""), "parse_module(): Invalid LLVM IR");
}


TEST_CASE("llvm 2") {
    LCompilers::LLVMEvaluator e;
    e.add_module(R"""(
@count = global i64 0

define i64 @f1()
{
    store i64 4, i64* @count
    %1 = load i64, i64* @count
    ret i64 %1
}
    )""");
    CHECK(e.execfn<int64_t>("f1") == 4);

    e.add_module(R"""(
@count = external global i64

define i64 @f2()
{
    %1 = load i64, i64* @count
    ret i64 %1
}
    )""");
    CHECK(e.execfn<int64_t>("f2") == 4);

    CHECK_THROWS_AS(e.add_module(R"""(
define i64 @f3()
{
    ; FAIL: @count is not defined
    %1 = load i64, i64* @count
    ret i64 %1
}
        )"""), LCompilers::LCompilersException);
}

TEST_CASE("llvm 3") {
    LCompilers::LLVMEvaluator e;
    e.add_module(R"""(
@count = global i64 5
    )""");

    e.add_module(R"""(
@count = external global i64

define i64 @f1()
{
    %1 = load i64, i64* @count
    ret i64 %1
}

define void @inc()
{
    %1 = load i64, i64* @count
    %2 = add i64 %1, 1
    store i64 %2, i64* @count
    ret void
}
    )""");
    CHECK(e.execfn<int64_t>("f1") == 5);

//    e.execfn<void>("inc");
//    CHECK(e.execfn<int64_t>("f1") == 6);
/*    e.execfn<void>("inc");
    CHECK(e.execfn<int64_t>("f1") == 7);
    */
/*
    e.add_module(R"""(
@count = external global i64

define void @inc2()
{
    %1 = load i64, i64* @count
    %2 = add i64 %1, 2
    store i64 %2, i64* @count
    ret void
}
    )""");
    CHECK(e.execfn<int64_t>("f1") == 7);
    e.execfn<void>("inc2");
    CHECK(e.execfn<int64_t>("f1") == 9);
    e.execfn<void>("inc2");
    CHECK(e.execfn<int64_t>("f1") == 11);
    e.execfn<void>("inc2");
    CHECK(e.execfn<int64_t>("f1") == 13);

    // Test that we can have another independent LLVMEvaluator and use both at
    // the same time:
    LFortran::LLVMEvaluator e2;
    e2.add_module(R"""(
@count = global i64 5

define i64 @f1()
{
    %1 = load i64, i64* @count
    ret i64 %1
}

define void @inc()
{
    %1 = load i64, i64* @count
    %2 = add i64 %1, 1
    store i64 %2, i64* @count
    ret void
}
    )""");

    CHECK(e2.execfn<int64_t>("f1") == 5);
    e2.execfn<void>("inc");
    CHECK(e2.execfn<int64_t>("f1") == 6);
    e2.execfn<void>("inc");
    CHECK(e2.execfn<int64_t>("f1") == 7);

    CHECK(e.execfn<int64_t>("f1") == 12);
    e2.execfn<void>("inc");
    CHECK(e2.execfn<int64_t>("f1") == 8);
    CHECK(e.execfn<int64_t>("f1") == 12);
    e.execfn<void>("inc2");
    CHECK(e2.execfn<int64_t>("f1") == 8);
    CHECK(e.execfn<int64_t>("f1") == 14);
*/
}

TEST_CASE("llvm 4") {
    LCompilers::LLVMEvaluator e;
    e.add_module(R"""(
@count = global i64 5

define i64 @f1()
{
    %1 = load i64, i64* @count
    ret i64 %1
}

define void @inc()
{
    %1 = load i64, i64* @count
    %2 = add i64 %1, 1
    store i64 %2, i64* @count
    ret void
}
)""");
    CHECK(e.execfn<int64_t>("f1") == 5);
/*    e.execfn<void>("inc");
    CHECK(e.execfn<int64_t>("f1") == 6);
    e.execfn<void>("inc");
    CHECK(e.execfn<int64_t>("f1") == 7);
    */
/*
    e.add_module(R"""(
declare void @inc()

define void @inc2()
{
    call void @inc()
    call void @inc()
    ret void
}
)""");
    CHECK(e.execfn<int64_t>("f1") == 7);
    e.execfn<void>("inc2");
    CHECK(e.execfn<int64_t>("f1") == 9);
    e.execfn<void>("inc");
    CHECK(e.execfn<int64_t>("f1") == 10);
    e.execfn<void>("inc2");
    CHECK(e.execfn<int64_t>("f1") == 12);

    CHECK_THROWS_AS(e.add_module(R"""(
define void @inc2()
{
    ; FAIL: @inc is not defined
    call void @inc()
    call void @inc()
    ret void
}
        )"""), LFortran::LCompilersException);
        */
}

TEST_CASE("llvm array 1") {
    LCompilers::LLVMEvaluator e;
    e.add_module(R"""(
; Sum the three elements in %a
define i64 @sum3(i64* %a)
{
    %a1addr = getelementptr i64, i64* %a, i64 0
    %a1 = load i64, i64* %a1addr

    %a2addr = getelementptr i64, i64* %a, i64 1
    %a2 = load i64, i64* %a2addr

    %a3addr = getelementptr i64, i64* %a, i64 2
    %a3 = load i64, i64* %a3addr

    %tmp = add i64 %a2, %a1
    %r = add i64 %a3, %tmp

    ret i64 %r
}


define i64 @f()
{
    %a = alloca [3 x i64]

    %a1 = getelementptr [3 x i64], [3 x i64]* %a, i64 0, i64 0
    store i64 1, i64* %a1

    %a2 = getelementptr [3 x i64], [3 x i64]* %a, i64 0, i64 1
    store i64 2, i64* %a2

    %a3 = getelementptr [3 x i64], [3 x i64]* %a, i64 0, i64 2
    store i64 3, i64* %a3

    %r = call i64 @sum3(i64* %a1)
    ret i64 %r
}
    )""");
    CHECK(e.execfn<int64_t>("f") == 6);
}

TEST_CASE("llvm array 2") {
    LCompilers::LLVMEvaluator e;
    e.add_module(R"""(
%array = type {i64, [3 x i64]}

; Sum the three elements in %a
define i64 @sum3(%array* %a)
{
    %a1addr = getelementptr %array, %array* %a, i64 0, i32 1, i64 0
    %a1 = load i64, i64* %a1addr

    %a2addr = getelementptr %array, %array* %a, i64 0, i32 1, i64 1
    %a2 = load i64, i64* %a2addr

    %a3addr = getelementptr %array, %array* %a, i64 0, i32 1, i64 2
    %a3 = load i64, i64* %a3addr

    %tmp = add i64 %a2, %a1
    %r = add i64 %a3, %tmp

    ret i64 %r
}


define i64 @f()
{
    %a = alloca %array

    %idx0 = getelementptr %array, %array* %a, i64 0, i32 0
    store i64 3, i64* %idx0

    %idx1 = getelementptr %array, %array* %a, i64 0, i32 1, i64 0
    store i64 1, i64* %idx1
    %idx2 = getelementptr %array, %array* %a, i64 0, i32 1, i64 1
    store i64 2, i64* %idx2
    %idx3 = getelementptr %array, %array* %a, i64 0, i32 1, i64 2
    store i64 3, i64* %idx3

    %r = call i64 @sum3(%array* %a)
    ret i64 %r
}
    )""");
    //CHECK(e.execfn<int64_t>("f") == 6);
}

int f(int a, int b) {
    return a+b;
}

TEST_CASE("llvm callback 0") {
    LCompilers::LLVMEvaluator e;
    std::string addr = std::to_string((int64_t)f);
    e.add_module(R"""(
define i64 @addrcaller(i64 %a, i64 %b)
{
    %f = inttoptr i64 )""" + addr + R"""( to i64 (i64, i64)*
    %r = call i64 %f(i64 %a, i64 %b)
    ret i64 %r
}

define i64 @f1()
{
    %r = call i64 @addrcaller(i64 2, i64 3)
    ret i64 %r
}
    )""");
    CHECK(e.execfn<int64_t>("f1") == 5);
}


TEST_CASE("ASR -> LLVM 1") {
    std::string source = R"(function f()
integer :: f
f = 5
end function)";

    // Src -> AST
    Allocator al(4*1024);
    LCompilers::diag::Diagnostics diagnostics;
    CompilerOptions compiler_options;
    compiler_options.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    LCompilers::LFortran::AST::TranslationUnit_t* tu = TRY(LCompilers::LFortran::parse(al, source,
        diagnostics, compiler_options));
    LCompilers::LFortran::AST::ast_t* ast = tu->m_items[0];
    CHECK(LCompilers::LFortran::pickle(*ast) == "(Function f [] [] () () () [(Declaration (AttrType TypeInteger [] () () None) [] [(f [] [] () () None ())] ()) (Assignment 0 f 5 ())] [] [])");

    // AST -> ASR
    LCompilers::SymbolTable::reset_global_counter();
    LCompilers::LocationManager lm;
    LCompilers::ASR::TranslationUnit_t* asr = TRY(LCompilers::LFortran::ast_to_asr(al, *tu,
        diagnostics, nullptr, false, compiler_options, lm));
    CHECK(LCompilers::pickle(*asr) == "(TranslationUnit (SymbolTable 1 {f: (Function (SymbolTable 2 {f: (Variable 2 f [] ReturnVar () () Default (Integer 4) () Source Public Required .false. .false. .false. () .false. .false. NotMethod () [] () .false.)}) f (FunctionType [] (Integer 4) Source Implementation () .false. .false. .false. .false. .false. [] .false. Host) [] [] [(Assignment (Var 2 f) (IntegerConstant 5 (Integer 4) Decimal) () .false. .false. .false. .false.)] (Var 2 f) Public .true. .true. ())}) [] ())");

    // ASR -> LLVM
    LCompilers::LLVMEvaluator e;
    LCompilers::PassManager lpm;
    lpm.use_default_passes();
    CompilerOptions co;
    co.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    co.platform = LCompilers::get_platform();
    LCompilers::Result<std::unique_ptr<LCompilers::LLVMModule>>
        res = LCompilers::asr_to_llvm(*asr, diagnostics, e.get_context(),
            e.get_target_config(), al, lpm, co, "f", "", "", lm);
    REQUIRE(res.ok);
    std::unique_ptr<LCompilers::LLVMModule> m = std::move(res.result);
    //std::cout << "Module:" << std::endl;
    //std::cout << m->str() << std::endl;

    // LLVM -> Machine code -> Execution
    e.add_module(std::move(m));
    CHECK(e.execfn<int32_t>("f") == 5);
}

TEST_CASE("ASR -> LLVM 2") {
    std::string source = R"(function f()
integer :: f
f = 4
end function)";

    // Src -> AST
    Allocator al(4*1024);
    LCompilers::diag::Diagnostics diagnostics;
    CompilerOptions compiler_options;
    compiler_options.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    LCompilers::LFortran::AST::TranslationUnit_t* tu = TRY(LCompilers::LFortran::parse(al, source,
        diagnostics, compiler_options));
    LCompilers::LFortran::AST::ast_t* ast = tu->m_items[0];
    CHECK(LCompilers::LFortran::pickle(*ast) == "(Function f [] [] () () () [(Declaration (AttrType TypeInteger [] () () None) [] [(f [] [] () () None ())] ()) (Assignment 0 f 4 ())] [] [])");

    // AST -> ASR
    LCompilers::LocationManager lm;
    LCompilers::ASR::TranslationUnit_t* asr = TRY(LCompilers::LFortran::ast_to_asr(al, *tu,
        diagnostics, nullptr, false, compiler_options, lm));
    CHECK(LCompilers::pickle(*asr) == "(TranslationUnit (SymbolTable 3 {f: (Function (SymbolTable 4 {f: (Variable 4 f [] ReturnVar () () Default (Integer 4) () Source Public Required .false. .false. .false. () .false. .false. NotMethod () [] () .false.)}) f (FunctionType [] (Integer 4) Source Implementation () .false. .false. .false. .false. .false. [] .false. Host) [] [] [(Assignment (Var 4 f) (IntegerConstant 4 (Integer 4) Decimal) () .false. .false. .false. .false.)] (Var 4 f) Public .true. .true. ())}) [] ())");
    // ASR -> LLVM
    LCompilers::LLVMEvaluator e;
    LCompilers::PassManager lpm;
    lpm.use_default_passes();
    LCompilers::Result<std::unique_ptr<LCompilers::LLVMModule>>
        res = LCompilers::asr_to_llvm(*asr, diagnostics, e.get_context(),
            e.get_target_config(), al, lpm, compiler_options, "f", "", "",
            lm);
    REQUIRE(res.ok);
    std::unique_ptr<LCompilers::LLVMModule> m = std::move(res.result);
    //std::cout << "Module:" << std::endl;
    //std::cout << m->str() << std::endl;

    // LLVM -> Machine code -> Execution
    e.add_module(std::move(m));
    CHECK(e.execfn<int32_t>("f") == 4);
}

#endif // __EMSCRIPTEN__

TEST_CASE("FortranEvaluator 1") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("integer :: i");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);
    r = e.evaluate2("i = 5");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::statement);
    r = e.evaluate2("i");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 5);
}

TEST_CASE("FortranEvaluator 2") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2(R"(real :: r
r = 3
r
)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::real4);
    CHECK(r.result.f32 == 3);
}

TEST_CASE("FortranEvaluator character result") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2("'hello'");
    REQUIRE(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::character);
    CHECK(r.result.str == "hello");
}

TEST_CASE("FortranEvaluator character function across cells") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2(
        "module display_result\n"
        "contains\n"
        "function wrap(data) result(result)\n"
        "character(len=*), intent(in) :: data\n"
        "character(len=:), allocatable :: result\n"
        "result = \"[\" // data // \"]\"\n"
        "end function\n"
        "end module\n");
    REQUIRE(r.ok);

    r = e.evaluate2(
        "use display_result\n"
        "wrap(\"hello\")");
    REQUIRE(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::character);
    CHECK(r.result.str == "[hello]");

    // A later cell must still see the clean function signature, not the
    // subroutine produced by the character-return lowering pass.
    r = e.evaluate2(
        "use display_result\n"
        "wrap(\"second cell\")");
    REQUIRE(r.ok);
    CHECK(r.result.str == "[second cell]");

    // The imported name remains available without another USE statement.
    r = e.evaluate2("wrap(\"third cell\")");
    REQUIRE(r.ok);
    CHECK(r.result.str == "[third cell]");
}

TEST_CASE("FortranEvaluator 3") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    e.evaluate2("integer :: i, j");
    e.evaluate2(R"(j = 0
do i = 1, 5
    j = j + i
end do
)");
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("j");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 15);
}

TEST_CASE("FortranEvaluator 4") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    e.evaluate2(R"(
integer function fn(i, j)
integer, intent(in) :: i, j
fn = i + j
end function
)");
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("fn(2, 3)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 5);

/*
    e.evaluate2(R"(
integer function fn(i, j)
integer, intent(in) :: i, j
fn = i - j
end function
)");
    r = e.evaluate2("fn(2, 3)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == -1);
*/
}

// FortranEvaluator 5 uses "fn_sub" (not "fn") to avoid an RTLD_GLOBAL collision
// with FortranEvaluator 4's "fn" function: Emscripten side-module instantiation
// fails with a LinkError if two WASM modules export the same name with different
// function signatures.
TEST_CASE("FortranEvaluator 5") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    e.evaluate2(R"(
integer subroutine fn_sub(i, j, r)
integer, intent(in) :: i, j
integer, intent(out) :: r
r = i + j
end subroutine
)");
    e.evaluate2("integer :: r");
    e.evaluate2("call fn_sub(2, 3, r)");
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("r");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 5);

/*
    e.evaluate2(R"(
integer subroutine fn(i, j, r)
integer, intent(in) :: i, j
integer, intent(out) :: r
r = i - j
end subroutine
)");
    e.evaluate2("call fn(2, 3, r)");
    r = e.evaluate2("r");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == -1);
    */
}

TEST_CASE("FortranEvaluator 6") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);

    LCompilers::LocationManager lm;
    LCompilers::PassManager lpm;
    lpm.use_default_passes();
    {
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = "input.f90";
        lm.files.push_back(fl);
    }
    LCompilers::diag::Diagnostics diagnostics;

    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate("$", false, lm, lpm, diagnostics);
    CHECK(!r.ok);
    REQUIRE(diagnostics.diagnostics.size() >= 1);
    CHECK(diagnostics.diagnostics[0].stage == LCompilers::diag::Stage::Tokenizer);
    diagnostics.diagnostics.clear();

    r = e.evaluate("1x", false, lm, lpm, diagnostics);
    CHECK(!r.ok);
    REQUIRE(diagnostics.diagnostics.size() >= 1);
    CHECK(diagnostics.diagnostics[0].stage == LCompilers::diag::Stage::Parser);
    diagnostics.diagnostics.clear();

    r = e.evaluate("x = 'x'", false, lm, lpm, diagnostics);
    CHECK(!r.ok);
    REQUIRE(diagnostics.diagnostics.size() >= 1);
    CHECK(diagnostics.diagnostics[0].stage == LCompilers::diag::Stage::Semantic);
    diagnostics.diagnostics.clear();
}

TEST_CASE("FortranEvaluator 6 importing modules") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);

    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2(R"(module funcmod
    implicit none

    contains

    function add(x,y)
        real :: x, y
        real :: add
        add = x+y
    end function add
    function subtract(x,y)
        real :: x, y
        real :: subtract
        subtract = x-y
    end function subtract
end module funcmod)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);

    r = e.evaluate2("use funcmod, only : add, subtract");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);

    r = e.evaluate2("add(2.0, 5.0)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::real4);
    CHECK(std::abs(r.result.f32 - 7.0) <= 1e-8 );

    r = e.evaluate2("subtract(2.0, 5.0)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::real4);
    CHECK(std::abs(r.result.f32 - (-3.0)) <= 1e-8 );
}

#ifndef __EMSCRIPTEN__

// Tests passing the complex struct by reference
TEST_CASE("llvm complex type") {
    LCompilers::LLVMEvaluator e;
    e.add_module(R"""(
%complex = type { float, float }

define float @sum2(%complex* %a)
{
    %a1addr = getelementptr %complex, %complex* %a, i32 0, i32 0
    %a1 = load float, float* %a1addr

    %a2addr = getelementptr %complex, %complex* %a, i32 0, i32 1
    %a2 = load float, float* %a2addr

    %r = fadd float %a2, %a1

    ret float %r
}

define float @f()
{
    %a = alloca %complex

    %a1 = getelementptr %complex, %complex* %a, i32 0, i32 0
    store float 3.5, float* %a1

    %a2 = getelementptr %complex, %complex* %a, i32 0, i32 1
    store float 4.5, float* %a2

    %r = call float @sum2(%complex* %a)
    ret float %r
}
    )""");
    CHECK(std::abs(e.execfn<float>("f") - 8) < 1e-6);
}

// Tests passing the complex struct by value
TEST_CASE("llvm complex type value") {
    LCompilers::LLVMEvaluator e;
    e.add_module(R"""(
%complex = type { float, float }

define float @sum2(%complex %a_value)
{
    %a = alloca %complex
    store %complex %a_value, %complex* %a

    %a1addr = getelementptr %complex, %complex* %a, i32 0, i32 0
    %a1 = load float, float* %a1addr

    %a2addr = getelementptr %complex, %complex* %a, i32 0, i32 1
    %a2 = load float, float* %a2addr

    %r = fadd float %a2, %a1
    ret float %r
}

define float @f()
{
    %a = alloca %complex

    %a1 = getelementptr %complex, %complex* %a, i32 0, i32 0
    store float 3.5, float* %a1

    %a2 = getelementptr %complex, %complex* %a, i32 0, i32 1
    store float 4.5, float* %a2

    %ap = load %complex, %complex* %a
    %r = call float @sum2(%complex %ap)
    ret float %r
}
    )""");
//    CHECK(std::abs(e.execfn<float>("f") - 8) < 1e-6);
}

// Tests passing boolean by reference
TEST_CASE("llvm boolean type") {
    LCompilers::LLVMEvaluator e;
    e.add_module(R"""(

define i1 @and_func(i1* %p, i1* %q)
{
    %pval = load i1, i1* %p
    %qval = load i1, i1* %q

    %r = and i1 %pval, %qval
    ret i1 %r
}

define i1 @b()
{
    %p = alloca i1
    %q = alloca i1

    store i1 1, i1* %p
    store i1 0, i1* %q

    %r = call i1 @and_func(i1* %p, i1* %q)

    ret i1 %r
}
    )""");
    CHECK(e.execfn<bool>("b") == false);
}

// Tests passing boolean by value
TEST_CASE("llvm boolean type") {
    LCompilers::LLVMEvaluator e;
    e.add_module(R"""(

define i1 @and_func(i1 %p, i1 %q)
{
    %r = and i1 %p, %q
    ret i1 %r
}

define i1 @b()
{
    %p = alloca i1
    %q = alloca i1

    store i1 1, i1* %p
    store i1 0, i1* %q

    %pval = load i1, i1* %p
    %qval = load i1, i1* %q

    %r = call i1 @and_func(i1 %pval, i1 %qval)

    ret i1 %r
}
    )""");
    CHECK(e.execfn<bool>("b") == false);
}

// Tests pointers
TEST_CASE("llvm pointers 1") {
    LCompilers::LLVMEvaluator e;
    e.add_module(R"""(
@r = global i64 0

define i64 @f()
{
    store i64 8, i64* @r

    ; Dereferences the pointer %r
    ;%rval = load i64, i64* @r

    %raddr = ptrtoint i64* @r to i64

    ret i64 %raddr
}
    )""");
    int64_t r = e.execfn<int64_t>("f");
    CHECK(r != 8);
    int64_t *p = (int64_t*)r;
    CHECK(*p == 8);
}

TEST_CASE("llvm pointers 2") {
    LCompilers::LLVMEvaluator e;
    e.add_module(R"""(
@r = global float 0.0

define i64 @f()
{
    store float 8.0, float* @r

    %raddr = ptrtoint float* @r to i64

    ret i64 %raddr
}
    )""");
    int64_t r = e.execfn<int64_t>("f");
    float *p = (float *)r;
    CHECK(std::abs(*p - 8) < 1e-6);
}

TEST_CASE("llvm pointers 3") {
    LCompilers::LLVMEvaluator e;
    e.add_module(R"""(
; Takes a variable and returns a pointer to it
define i64 @pointer_reference(float* %var)
{
    %ret = ptrtoint float* %var to i64
    ret i64 %ret
}

; Takes a pointer and returns the value of what it points to
define float @pointer_dereference(i64 %p)
{
    %p2 = inttoptr i64 %p to float*
    %ret = load float, float* %p2
    ret float %ret
}

define float @f()
{
    %var = alloca float
    store float 8.0, float* %var

    %pointer_val = call i64 @pointer_reference(float* %var)

    ; Save the pointer to a variable
    %pointer_var = alloca i64
    store i64 %pointer_val, i64* %pointer_var
    ; Extract value out of the pointer %pointer_var
    %pointer_val2 = load i64, i64* %pointer_var

    %ret = call float @pointer_dereference(i64 %pointer_val2)

    ret float %ret
}
    )""");
    float r = e.execfn<float>("f");
    CHECK(std::abs(r - 8) < 1e-6);
}

TEST_CASE("llvm pointers 4") {
    LCompilers::LLVMEvaluator e;
    e.add_module(R"""(
; Takes a variable and returns a pointer to it
define float* @pointer_reference(float* %var)
{
    ret float* %var
}

define float @pointer_dereference(float* %var)
{
    %ret = load float, float* %var
    ret float %ret
}

define float @f()
{
    %var = alloca float
    store float 8.0, float* %var

    %pointer_val = call float* @pointer_reference(float* %var)

    ; Save the pointer to a variable
    %pointer_var = alloca float*
    store float* %pointer_val, float** %pointer_var
    ; Extract value out of the pointer %pointer_var
    %pointer_val2 = load float*, float** %pointer_var

    %ret = call float @pointer_dereference(float* %pointer_val2)

    ret float %ret
}
    )""");
    float r = e.execfn<float>("f");
    CHECK(std::abs(r - 8) < 1e-6);
}

#endif // __EMSCRIPTEN__

TEST_CASE("FortranEvaluator 7") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("integer :: i = 5");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);
    r = e.evaluate2("i");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 5);
}

TEST_CASE("FortranEvaluator 8") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("real :: a = 3.5");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);
    r = e.evaluate2("a");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::real4);
    CHECK(r.result.f32 == 3.5);
}

// "ad" avoids an RTLD_GLOBAL collision with FortranEvaluator 8's "a" (float):
// both tests are in the same process and RTLD_GLOBAL uses first-defined-wins,
// so GOT.mem.a set by the f32 test would shadow the f64 global here.
TEST_CASE("FortranEvaluator 8 double") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("real(8) :: ad = 3.5");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);
    r = e.evaluate2("ad");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::real8);
    CHECK(r.result.f64 == 3.5);
}

TEST_CASE("FortranEvaluator 9 single complex") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    if (cu.platform == LCompilers::Platform::Linux ||
            cu.platform == LCompilers::Platform::Windows) {
        FortranEvaluator e(cu);
        LCompilers::Result<FortranEvaluator::EvalResult>
        r = e.evaluate2("(2.5_4, 3.5_4)");
        REQUIRE(r.ok);
        REQUIRE(r.result.type == FortranEvaluator::EvalResult::complex4);
        CHECK(r.result.c32.re == 2.5);
        CHECK(r.result.c32.im == 3.5);
    }
}

TEST_CASE("FortranEvaluator 9 double complex") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("(2.5_8, 3.5_8)");
    REQUIRE(r.ok);
    REQUIRE(r.result.type == FortranEvaluator::EvalResult::complex8);
    CHECK(r.result.c64.re == 2.5);
    CHECK(r.result.c64.im == 3.5);
}

#ifndef __EMSCRIPTEN__
TEST_CASE("FortranEvaluator Windows complex result ABI") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    // Exercise Windows' i64 and output-pointer entry points on native JITs.
    // These primitive signatures also use the host's scalar/pointer ABI.
    cu.platform = LCompilers::Platform::Windows;
    FortranEvaluator e(cu);
    for (bool swapped : {false, true}) {
        CAPTURE(swapped);
        auto r = e.evaluate2(swapped ? "(-2.5_4, 1.25_4)" : "(1.25_4, -2.5_4)");
        REQUIRE(r.ok);
        REQUIRE(r.result.type == FortranEvaluator::EvalResult::complex4);
        CHECK(r.result.c32.re == (swapped ? -2.5f : 1.25f));
        CHECK(r.result.c32.im == (swapped ? 1.25f : -2.5f));
    }
    for (bool swapped : {false, true}) {
        CAPTURE(swapped);
        auto r = e.evaluate2(swapped ? "(-2.5_8, 1.25_8)" : "(1.25_8, -2.5_8)");
        REQUIRE(r.ok);
        REQUIRE(r.result.type == FortranEvaluator::EvalResult::complex8);
        CHECK(r.result.c64.re == (swapped ? -2.5 : 1.25));
        CHECK(r.result.c64.im == (swapped ? 1.25 : -2.5));
    }
}
#endif

TEST_CASE("FortranEvaluator logical 1") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("logical :: i");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);
    r = e.evaluate2("i = .true.");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::statement);
    r = e.evaluate2("i");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::boolean);
    CHECK(r.result.b);
}

TEST_CASE("FortranEvaluator character comparison") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    // Comparing character dummy arguments calls the runtime's str_compare,
    // which the JIT must resolve from the runtime library on every platform.
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2(
        "module str_order\n"
        "contains\n"
        "logical function before(x, y)\n"
        "character(len=*), intent(in) :: x, y\n"
        "before = x < y\n"
        "end function\n"
        "end module\n");
    REQUIRE(r.ok);
    r = e.evaluate2("use str_order\nbefore('abc', 'abd')");
    REQUIRE(r.ok);
    REQUIRE(r.result.type == FortranEvaluator::EvalResult::boolean);
    CHECK(r.result.b);
    r = e.evaluate2("before('abd', 'abc')");
    REQUIRE(r.ok);
    REQUIRE(r.result.type == FortranEvaluator::EvalResult::boolean);
    CHECK(!r.result.b);
}

TEST_CASE("FortranEvaluator logical 2") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("logical :: i");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);
    r = e.evaluate2("i = .false.");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::statement);
    r = e.evaluate2("i");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::boolean);
    CHECK(!r.result.b);
}

TEST_CASE("FortranEvaluator logical 3") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2(".false. .or. .true.");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::boolean);
    CHECK(r.result.b);
    r = e.evaluate2(".false. .and. .true.");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::boolean);
    CHECK(!r.result.b);
    r = e.evaluate2("logical :: i");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);
    r = e.evaluate2("i = .false.");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::statement);
    r = e.evaluate2("i .or. .true.");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::boolean);
    CHECK(r.result.b);
}

TEST_CASE("FortranEvaluator logical 4") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2(R"(
function is_even(n) result(result)
integer, intent(in) :: n
logical :: result
result = mod(n, 2) == 0
end function is_even
)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);
    r = e.evaluate2("is_even(3)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::boolean);
    CHECK(!r.result.b);
    r = e.evaluate2("is_even(4)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::boolean);
    CHECK(r.result.b);
}

TEST_CASE("FortranEvaluator integer kind 1") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("integer(4) :: i");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);
    r = e.evaluate2("i = 5");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::statement);
    r = e.evaluate2("i");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 5);
}

TEST_CASE("FortranEvaluator integer kind 2") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("integer(8) :: i");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);
    r = e.evaluate2("i = 5");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::statement);
    r = e.evaluate2("i");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer8);
    CHECK(r.result.i64 == 5);
}

TEST_CASE("FortranEvaluator Array 1") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("integer :: i(10)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);
    r = e.evaluate2("print *, i");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::statement);
}

TEST_CASE("FortranEvaluator Array 2") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("integer :: x(3)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);
    r = e.evaluate2("x = 5");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::statement);
    r = e.evaluate2("print *, x");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::statement);
}

TEST_CASE("FortranEvaluator re-declaration 1") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("integer :: i");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);
    r = e.evaluate2("i = 5");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::statement);
    r = e.evaluate2("i");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 5);

/*
    r = e.evaluate2("integer :: i");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);
    r = e.evaluate2("i = 6");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::statement);
    r = e.evaluate2("i");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 6);
*/
}

// "fn_redecl" avoids an RTLD_GLOBAL collision with FortranEvaluator 4's "fn"
// function (different arity → LinkError at WASM side-module instantiation).
TEST_CASE("FortranEvaluator re-declaration 2") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2(R"(
integer function fn_redecl(i)
integer, intent(in) :: i
fn_redecl = i+1
end function
)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);
    r = e.evaluate2("fn_redecl(3)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 4);

/*
    r = e.evaluate2(R"(
integer function fn(i)
integer, intent(in) :: i
fn = i-1
end function
)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);
    r = e.evaluate2("fn(3)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 2);
*/
}

TEST_CASE("FortranEvaluator asr verify 1") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2(R"(
pure function double(x) result(r)
    integer, intent(in) :: x
    integer :: r
    r = 2 * x
end function double
)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);
    r = e.evaluate2(R"(
subroutine s(n, x)
    integer, intent(in) :: n
    integer, intent(inout) :: x
    x = double(n)
end subroutine s
)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);
    r = e.evaluate2("integer :: x");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);
    r = e.evaluate2("x = double(1)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::statement);
    r = e.evaluate2("x");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 2);
    r = e.evaluate2("call s(x, x)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::statement);
    r = e.evaluate2("x");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 4);
}

TEST_CASE("FortranEvaluator asr verify 2") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2(R"(
pure function id(x) result(r)
    integer, intent(in) :: x
    integer :: r
    r = x
end function id
)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);
    r = e.evaluate2(R"(
subroutine sa(l, a)
    integer, intent(in) :: l
    integer, intent(inout) :: a(id(l))

    integer :: i

    do i = 1, size(a)
        a(i) = a(i) + 1
    end do
end subroutine sa
)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);
    r = e.evaluate2("integer :: arr(3)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);
    r = e.evaluate2("arr = 0");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::statement);
    r = e.evaluate2("call sa(3, arr)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::statement);
    r = e.evaluate2("arr(1)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 1);
    r = e.evaluate2("arr(2)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 1);
    r = e.evaluate2("arr(3)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 1);
}

TEST_CASE("FortranEvaluator asr verify 3") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2(R"(
function add(x, y) result(r)
    real, intent(in) :: x
    real, intent(in) :: y
    real :: r
    r = x + y
end function add
)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);
    r = e.evaluate2(R"(
function sub(x, y) result(r)
    real, intent(in) :: x
    real, intent(in) :: y
    real :: r
    r = add(x, -y)
end function sub
)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);
    r = e.evaluate2("add(2.0, 3.0)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::real4);
    CHECK(r.result.f32 == 5.0);
    r = e.evaluate2("sub(2.0, 3.0)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::real4);
    CHECK(r.result.f32 == -1.0);
}

#ifndef __EMSCRIPTEN__
TEST_CASE("llvm ir 1") {
    LCompilers::LLVMEvaluator e;
    std::string file_name = std::string(LFORTRAN_PROJECT_SOURCE_DIR) + "/src/lfortran/tests/ir.ll";
    std::ifstream infile(file_name);
    if (!infile.good()) {
        std::string error_msg = "File '" + file_name + "' doesn't exist or isn't readable";
        FAIL(error_msg);
    }
    // `file_name` evaluates to something like:
    // "/Users/gxyd/OpenSource/lfortran/lfortran-${version}/src/lfortran/tests/ir.ll",
    // where `version` isn't a local variable
    CHECK_THROWS_AS(e.parse_module2("", file_name), LCompilers::LCompilersException);
    CHECK_THROWS_WITH(e.parse_module2("", file_name), "parse_module(): Invalid LLVM IR");
}
#endif // __EMSCRIPTEN__

// This test does not work on Windows yet
// https://github.com/lfortran/lfortran/issues/913
#if !defined(_WIN32)
TEST_CASE("FortranEvaluator 10 trig functions") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("sin(1.0)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::real4);
    CHECK(std::abs(r.result.f32 - 0.8414709848078965) < 1e-7);
    /*
    r = e.evaluate2("sin(1.d0)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::real8);
    CHECK(std::abs(r.result.f64 - 0.8414709848078965) < 1e-14);
    r = e.evaluate2("cos(1.0)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::real4);
    CHECK(std::abs(r.result.f32 - 0.5403023058681398) < 1e-7);
    r = e.evaluate2("cos(1.d0)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::real8);
    CHECK(std::abs(r.result.f64 - 0.5403023058681398) < 1e-14);
    */
}
#endif

TEST_CASE("FortranEvaluator kind parameter from global symtab") {
    // Regression test: declaring a parameter `dp = kind(1.0d0)` and then
    // using it in a subsequent declaration `real(dp) :: x(5)` previously
    // crashed in interactive mode because extract_kind() unconditionally
    // down-casted the global symtab's asr_owner (a TranslationUnit_t) to
    // ASR::symbol_t.
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("integer, parameter :: dp = kind(1.0d0)");
    CHECK(r.ok);
    r = e.evaluate2("real(dp) :: x(5)");
    CHECK(r.ok);
}

#if !defined(__EMSCRIPTEN__)
// TODO: The 80 MB stack allocation (real(dp) :: x(10**7)) exceeds the WASM
// shadow-stack budget and causes a memory-access-out-of-bounds at runtime.
TEST_CASE("FortranEvaluator pass_array_by_data on global random_number") {
    // Regression test: in interactive mode the global symbol table persists
    // across evaluate2 calls, and the pass_array_by_data pass would re-add
    // the same generated procedure name on each subsequent evaluation,
    // tripping the SymbolTable::add_symbol assertion.
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("integer, parameter :: n = 10**7");
    CHECK(r.ok);
    r = e.evaluate2("integer, parameter :: dp = kind(1.0d0)");
    CHECK(r.ok);
    r = e.evaluate2("real(kind=dp) :: x(n)");
    CHECK(r.ok);
    r = e.evaluate2("call random_number(x)");
    CHECK(r.ok);
    r = e.evaluate2("call random_number(x)");
    CHECK(r.ok);
    r = e.evaluate2("print*,sum(x)/size(x)");
    CHECK(r.ok);
    r = e.evaluate2("print*,sum(x)/size(x)");
    CHECK(r.ok);
}
#endif

#ifdef __EMSCRIPTEN__
TEST_CASE("FortranEvaluator iso_c_binding") {
    // Regression test for JupyterLite wasm kernel: "use iso_c_binding" must
    // resolve to the preloaded .mod file at /lib/lfortran_intrinsic_iso_c_binding.mod
    // (set via --preload-file at link time; on native builds get_runtime_library_dir()
    // returns the installed lib path directly).
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("use iso_c_binding, only: c_int");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);
    r = e.evaluate2("integer(c_int) :: n_cint");
    CHECK(r.ok);
    r = e.evaluate2("n_cint = 7_c_int");
    CHECK(r.ok);
    r = e.evaluate2("n_cint");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 7);
}

TEST_CASE("WasmLFortranExecutor basic") {
    // Directly exercise WasmLFortranExecutor bypassing FortranEvaluator
    LCompilers::WasmLFortranExecutor we;
    auto m = we.parse_module2(R"""(
define i64 @__lfortran_evaluate_1()
{
    ret i64 42
}
    )""", "test.ll");
    we.add_module(std::move(m), 1);
    CHECK(we.execfn<int64_t>("__lfortran_evaluate_1") == 42);
}
#endif

TEST_CASE("FortranEvaluator program unit in a cell") {
    // A cell holding a complete program unit must run it. Before this was
    // fixed the program was compiled and never called, so the cell produced
    // no output at all -- prints, and any display_data, silently did nothing.
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);

    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2(
        "module counter_mod\n"
        "implicit none\n"
        "integer :: counter = 0\n"
        "end module counter_mod\n");
    CHECK(r.ok);

    r = e.evaluate2(
        "program bump\n"
        "use counter_mod\n"
        "implicit none\n"
        "counter = counter + 1\n"
        "end program bump\n");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::statement);

    r = e.evaluate2("use counter_mod\ncounter\n");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 1);

    // Re-running the same cell is the ordinary notebook loop: the program is
    // redefined under the same name and runs again.
    r = e.evaluate2(
        "program bump\n"
        "use counter_mod\n"
        "implicit none\n"
        "counter = counter + 1\n"
        "end program bump\n");
    CHECK(r.ok);

    r = e.evaluate2("use counter_mod\ncounter\n");
    CHECK(r.ok);
    CHECK(r.result.i32 == 2);

    // A program under a different name also runs, and loose statements keep
    // working afterwards.
    r = e.evaluate2(
        "program bump2\n"
        "use counter_mod\n"
        "implicit none\n"
        "counter = counter + 10\n"
        "end program bump2\n");
    CHECK(r.ok);

    r = e.evaluate2("use counter_mod\ncounter\n");
    CHECK(r.ok);
    CHECK(r.result.i32 == 12);

    r = e.evaluate2("integer :: leftover\n");
    CHECK(r.ok);
    r = e.evaluate2("leftover = 5\n");
    CHECK(r.ok);
    r = e.evaluate2("leftover\n");
    CHECK(r.ok);
    CHECK(r.result.i32 == 5);
}

TEST_CASE("FortranEvaluator array-argument module procedure across cells") {
    // ASR passes rewrite symbols in place. In interactive mode the symbol
    // table is the session state, so a pass that specialises a procedure --
    // pass_array_by_data, for an assumed-shape array argument -- used to
    // leave the module holding only the mangled specialisation, and the next
    // cell failed with "Function 'sf' not found".
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);

    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2(
        "module mf\n"
        "implicit none\n"
        "contains\n"
        "integer function sf(a)\n"
        "real, intent(in) :: a(:)\n"
        "sf = size(a)\n"
        "end function sf\n"
        "end module mf\n");
    CHECK(r.ok);

    // called from a later cell
    r = e.evaluate2("use mf\nreal :: v(3)\nv = 1\n");
    CHECK(r.ok);
    r = e.evaluate2("sf(v)\n");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 3);

    // and from another cell again, with a different array
    r = e.evaluate2("real :: w(5)\nw = 2\n");
    CHECK(r.ok);
    r = e.evaluate2("sf(w)\n");
    CHECK(r.ok);
    CHECK(r.result.i32 == 5);

    // from inside a program unit too
    r = e.evaluate2(
        "program pf\n"
        "use mf\n"
        "implicit none\n"
        "real :: u(7)\n"
        "u = 3\n"
        "print *, sf(u)\n"
        "end program pf\n");
    CHECK(r.ok);
}

// Shadowing across interactive cells. Each cell is its own TranslationUnit,
// chained through SymbolTable::parent, so re-declaring a name in a later cell
// does not overwrite the earlier one: code compiled in earlier cells keeps
// using the binding that was in scope when it was compiled, and new code sees
// the new one. This mirrors what Python does in a notebook.

TEST_CASE("FortranEvaluator shadow a variable across cells") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    CHECK(e.evaluate2("integer :: i").ok);
    CHECK(e.evaluate2("i = 1").ok);
    // A function compiled now binds to the `i` that exists now.
    CHECK(e.evaluate2(R"(integer function old_i()
old_i = i
end function
)").ok);
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2("old_i()");
    CHECK(r.ok);
    CHECK(r.result.i32 == 1);

    // Re-declaring `i` shadows it; the old one stays alive.
    CHECK(e.evaluate2("integer :: i").ok);
    CHECK(e.evaluate2("i = 2").ok);
    r = e.evaluate2("i");
    CHECK(r.ok);
    CHECK(r.result.i32 == 2);
    r = e.evaluate2("old_i()");
    CHECK(r.ok);
    CHECK(r.result.i32 == 1);
}

TEST_CASE("FortranEvaluator shadow a function across cells") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    CHECK(e.evaluate2(R"(integer function f()
f = 1
end function
)").ok);
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2("f()");
    CHECK(r.ok);
    CHECK(r.result.i32 == 1);

    // Re-defining `f` shadows it: new code calls the new one.
    CHECK(e.evaluate2(R"(integer function f()
f = 2
end function
)").ok);
    r = e.evaluate2("f()");
    CHECK(r.ok);
    CHECK(r.result.i32 == 2);

    // The first `f` is still there. Fortran gives no way to name it once it
    // is shadowed, so this checks the symbol the first cell emitted: the two
    // definitions live under different names (the second cell qualifies its
    // symbols by its cell) and both are defined in the JIT.
    //
    // The WASM build executes through WasmLFortranExecutor and has no JIT to
    // look a symbol up in. `FortranEvaluator old and new function side by
    // side` covers the same ground there, in Fortran rather than by symbol.
#ifndef __EMSCRIPTEN__
    CHECK(e.get_llvm_evaluator().execfn<int32_t>("f") == 1);
#endif
}

TEST_CASE("FortranEvaluator shadow a module across cells") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    CHECK(e.evaluate2(R"(module m
implicit none
integer, parameter :: c = 1
end module
)").ok);
    CHECK(e.evaluate2(R"(integer function old_c()
use m, only: c
old_c = c
end function
)").ok);
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2("old_c()");
    CHECK(r.ok);
    CHECK(r.result.i32 == 1);

    CHECK(e.evaluate2(R"(module m
implicit none
integer, parameter :: c = 2
end module
)").ok);
    CHECK(e.evaluate2(R"(integer function new_c()
use m, only: c
new_c = c
end function
)").ok);
    r = e.evaluate2("new_c()");
    CHECK(r.ok);
    CHECK(r.result.i32 == 2);
    // The function compiled against the first `m` still sees c == 1.
    r = e.evaluate2("old_c()");
    CHECK(r.ok);
    CHECK(r.result.i32 == 1);
}

// A module declared in an earlier cell is in a parent scope, not in the cell
// being compiled. Resolving `use` has to look through the chain; otherwise a
// modfile is searched for on disk, of which there is none in a notebook.

TEST_CASE("FortranEvaluator module using another module across cells") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    CHECK(e.evaluate2(R"(module ma
implicit none
integer, parameter :: c = 5
end module
)").ok);
    CHECK(e.evaluate2(R"(module mb
use ma
implicit none
end module
)").ok);
    // A program unit resolves `use` through load_module(), which is the path
    // that used to report "modfile was not found" for ma, a dependency of mb.
    CHECK(e.evaluate2(R"(program p
use mb
implicit none
if (c /= 5) error stop
end program
)").ok);
}

TEST_CASE("FortranEvaluator re-exported module symbol across cells") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    // mtop re-exports msub's procedure: its scope holds an ExternalSymbol
    // pointing into msub. The copy taken for the next cell has to point at the
    // copy of msub, not at the original, or the symbol looks absent.
    CHECK(e.evaluate2(R"(module msub
implicit none
contains
    integer function twice(i)
        integer, intent(in) :: i
        twice = 2 * i
    end function
end module
module mtop
use msub
implicit none
end module
)").ok);
    CHECK(e.evaluate2(R"(program p
use mtop
implicit none
if (twice(3) /= 6) error stop
end program
)").ok);
}

TEST_CASE("FortranEvaluator optional argument across cells") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    // Calling this with the optional argument absent only works if the pass
    // that replaces optional arguments with presence flags has been applied to
    // the procedure in the earlier cell as well as to this cell's call.
    CHECK(e.evaluate2(R"(module mopt
implicit none
contains
    subroutine s(x, lbl)
        real, intent(in) :: x(:)
        character(len=*), intent(in), optional :: lbl
        if (present(lbl)) then
            if (x(1) /= 1.0) error stop
        else
            if (x(1) /= 1.0) error stop
        end if
    end subroutine
end module
)").ok);
    CHECK(e.evaluate2(R"(program p
use mopt
implicit none
real :: a(3)
a = 1.0
call s(a)
call s(a, "with label")
end program
)").ok);
}

TEST_CASE("FortranEvaluator re-run a cell calling an optional argument") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    CHECK(e.evaluate2(R"(module mopt2
implicit none
contains
    subroutine s(x, lbl)
        real, intent(in) :: x(:)
        character(len=*), intent(in), optional :: lbl
        if (x(1) /= 1.0) error stop
        if (present(lbl)) then
            if (len(lbl) == 0) error stop
        end if
    end subroutine
end module
)").ok);
    // Running the same cell twice has to keep working. The pass that replaces
    // optional arguments with presence flags rewrites the procedure in place,
    // so if it were let at the copy handed to the next cell, `lbl` would come
    // back as a required argument and this call would stop compiling.
    const char *cell = R"(program p
use mopt2
implicit none
real :: a(3)
a = 1.0
call s(a)
end program
)";
    CHECK(e.evaluate2(cell).ok);
    CHECK(e.evaluate2(cell).ok);
    CHECK(e.evaluate2(cell).ok);
}

TEST_CASE("FortranEvaluator common block across cells") {
    // The storage of a common block is created by the first cell that
    // declares the block. A later cell's view of it, in a program, a module,
    // a procedure or the global scope, refers to that storage: it does not
    // get a copy of its own.
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);

    CHECK(e.evaluate2(R"(program set_cells
implicit none
integer :: x, y
common /cells_blk/ x, y
x = 3
y = 4
end program set_cells
)").ok);
    CHECK(e.evaluate2(R"(module cells_mod
implicit none
integer :: a, b
common /cells_blk/ a, b
end module cells_mod
)").ok);
    LCompilers::Result<FortranEvaluator::EvalResult> r
        = e.evaluate2("use cells_mod\na + b\n");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 7);

    CHECK(e.evaluate2(R"(subroutine bump_cells()
implicit none
integer :: x, y
common /cells_blk/ x, y
x = x + 10
end subroutine bump_cells
)").ok);
    CHECK(e.evaluate2("call bump_cells()\n").ok);
    r = e.evaluate2("use cells_mod\na\n");
    CHECK(r.ok);
    CHECK(r.result.i32 == 13);

    CHECK(e.evaluate2("integer :: g1, g2\ncommon /cells_blk/ g1, g2\n").ok);
    r = e.evaluate2("g2\n");
    CHECK(r.ok);
    CHECK(r.result.i32 == 4);

    // The storage an earlier cell created cannot grow, nor be given initial
    // values.
    CHECK(!e.evaluate2(R"(program grow_cells
implicit none
integer :: z(3)
common /cells_blk/ z
z = 0
end program grow_cells
)").ok);
    CHECK(!e.evaluate2(R"(block data init_cells
implicit none
integer :: x, y
common /cells_blk/ x, y
data x /1/
end block data init_cells
)").ok);
}

TEST_CASE("FortranEvaluator call a procedure no earlier cell called") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    // `untouched` is never called in the cell that declares it. Dead-code
    // elimination would drop it there, leaving the next cell's call with no
    // definition to link to.
    CHECK(e.evaluate2(R"(module munused
implicit none
integer :: counter = 0
contains
    subroutine called_now()
        counter = counter + 1
    end subroutine
    subroutine untouched()
        counter = counter + 10
    end subroutine
end module

program p1
use munused
implicit none
call called_now()
if (counter /= 1) error stop
end program
)").ok);
    CHECK(e.evaluate2(R"(program p2
use munused
implicit none
call untouched()
if (counter /= 11) error stop
end program
)").ok);
}

TEST_CASE("FortranEvaluator shadow a subroutine across cells") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    CHECK(e.evaluate2("integer :: r").ok);
    CHECK(e.evaluate2(R"(subroutine sub()
r = 1
end subroutine
)").ok);
    // Compiled now, so it calls the `sub` that exists now.
    CHECK(e.evaluate2(R"(subroutine call_old_sub()
call sub()
end subroutine
)").ok);
    CHECK(e.evaluate2("call call_old_sub()").ok);
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2("r");
    CHECK(r.ok);
    CHECK(r.result.i32 == 1);

    // Re-defining `sub` shadows it: a call made now runs the new one.
    CHECK(e.evaluate2(R"(subroutine sub()
r = 2
end subroutine
)").ok);
    CHECK(e.evaluate2("call sub()").ok);
    r = e.evaluate2("r");
    CHECK(r.ok);
    CHECK(r.result.i32 == 2);

    // The subroutine compiled against the first `sub` still runs the first one.
    CHECK(e.evaluate2("call call_old_sub()").ok);
    r = e.evaluate2("r");
    CHECK(r.ok);
    CHECK(r.result.i32 == 1);
}

// Both bindings have to be reachable from a single cell: an accessor compiled
// against the old one and an accessor compiled against the new one, called
// side by side. Re-evaluating the old accessor then rebinds it to the new
// symbol, which is what a notebook user re-running a cell expects.

TEST_CASE("FortranEvaluator old and new variable side by side") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    const char *accessor = R"(integer function f1()
f1 = x
end function
)";
    CHECK(e.evaluate2("integer :: x").ok);
    CHECK(e.evaluate2("x = 1").ok);
    CHECK(e.evaluate2(accessor).ok);
    CHECK(e.evaluate2("integer :: x").ok);
    CHECK(e.evaluate2("x = 2").ok);
    CHECK(e.evaluate2(R"(integer function f2()
f2 = x
end function
)").ok);
    // One cell, both accessors: the two `x` are live at the same time.
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2("f1()");
    CHECK(r.ok);
    CHECK(r.result.i32 == 1);
    r = e.evaluate2("f2()");
    CHECK(r.ok);
    CHECK(r.result.i32 == 2);
    CHECK(e.evaluate2(R"(if (f1() /= 1) error stop
if (f2() /= 2) error stop
if (f1() == f2()) error stop
)").ok);

    // Re-evaluating f1 rebinds it to the `x` that is in scope now.
    CHECK(e.evaluate2(accessor).ok);
    r = e.evaluate2("f1()");
    CHECK(r.ok);
    CHECK(r.result.i32 == 2);
    CHECK(e.evaluate2("if (f1() /= f2()) error stop").ok);
}

TEST_CASE("FortranEvaluator old and new function side by side") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    const char *caller = R"(integer function g1()
g1 = f()
end function
)";
    CHECK(e.evaluate2(R"(integer function f()
f = 1
end function
)").ok);
    CHECK(e.evaluate2(caller).ok);
    CHECK(e.evaluate2(R"(integer function f()
f = 2
end function
)").ok);
    CHECK(e.evaluate2(R"(integer function g2()
g2 = f()
end function
)").ok);
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2("g1()");
    CHECK(r.ok);
    CHECK(r.result.i32 == 1);
    r = e.evaluate2("g2()");
    CHECK(r.ok);
    CHECK(r.result.i32 == 2);
    CHECK(e.evaluate2(R"(if (g1() /= 1) error stop
if (g2() /= 2) error stop
if (g1() == g2()) error stop
)").ok);

    CHECK(e.evaluate2(caller).ok);
    r = e.evaluate2("g1()");
    CHECK(r.ok);
    CHECK(r.result.i32 == 2);
    CHECK(e.evaluate2("if (g1() /= g2()) error stop").ok);
}

TEST_CASE("FortranEvaluator old and new module side by side") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    const char *accessor = R"(integer function a1()
use mshadow, only: c
a1 = c
end function
)";
    CHECK(e.evaluate2(R"(module mshadow
implicit none
integer, parameter :: c = 1
end module
)").ok);
    CHECK(e.evaluate2(accessor).ok);
    CHECK(e.evaluate2(R"(module mshadow
implicit none
integer, parameter :: c = 2
end module
)").ok);
    CHECK(e.evaluate2(R"(integer function a2()
use mshadow, only: c
a2 = c
end function
)").ok);
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2("a1()");
    CHECK(r.ok);
    CHECK(r.result.i32 == 1);
    r = e.evaluate2("a2()");
    CHECK(r.ok);
    CHECK(r.result.i32 == 2);
    // Both modules are usable at the same time, from one cell.
    CHECK(e.evaluate2(R"(if (a1() /= 1) error stop
if (a2() /= 2) error stop
if (a1() == a2()) error stop
)").ok);

    CHECK(e.evaluate2(accessor).ok);
    r = e.evaluate2("a1()");
    CHECK(r.ok);
    CHECK(r.result.i32 == 2);
    CHECK(e.evaluate2("if (a1() /= a2()) error stop").ok);
}

TEST_CASE("FortranEvaluator generic procedure from an earlier cell") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    // The copy of the first cell's generic interface has to resolve to the
    // copies of its specific procedures. Pointing at the originals selects a
    // procedure that this cell does not hold and never declares.
    CHECK(e.evaluate2(R"(module mgen
implicit none
interface twice
    module procedure twice_int
    module procedure twice_real
end interface
contains
    integer function twice_int(i)
        integer, intent(in) :: i
        twice_int = 2 * i
    end function
    real function twice_real(x)
        real, intent(in) :: x
        twice_real = 2 * x
    end function
end module
)").ok);
    CHECK(e.evaluate2(R"(program p
use mgen
implicit none
if (twice(3) /= 6) error stop
if (twice(1.5) /= 3.0) error stop
end program
)").ok);
}

TEST_CASE("FortranEvaluator a modfile module used by a cell's module") {
    // A module loaded from a modfile is held by the cell that first used it,
    // so a later cell resolves it through the chain and its module calls into
    // the copy. That copy has to be declared before this cell's own modules
    // are emitted, or the call has nothing to link to.
    const char *modsrc = R"(module mcellmod
implicit none
contains
subroutine say(i)
integer, intent(in) :: i
if (i /= 3) error stop
end subroutine
end module
)";
    {
        Allocator al(1024*1024);
        LCompilers::diag::Diagnostics diagnostics;
        CompilerOptions co;
        LCompilers::LocationManager lm;
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = "mcellmod.f90";
        lm.files.push_back(fl);
        lm.file_ends.push_back(LCompilers::LFortran::source_offset(std::strlen(modsrc)));
        LCompilers::LFortran::AST::TranslationUnit_t* ast = TRY(
            LCompilers::LFortran::parse(al, modsrc, diagnostics, co));
        LCompilers::ASR::TranslationUnit_t* asr = TRY(
            LCompilers::LFortran::ast_to_asr(al, *ast, diagnostics, nullptr,
                false, co, lm));
        std::ofstream out("mcellmod.mod", std::ios::binary);
        out << LCompilers::save_modfile(*asr, lm);
    }

    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    const char *cell = R"(module mcell
use mcellmod
implicit none
contains
subroutine show()
call say(3)
end subroutine
end module
program p
use mcell
implicit none
end program
)";
    CHECK(e.evaluate2(cell).ok);
    CHECK(e.evaluate2(cell).ok);
    std::remove("mcellmod.mod");
}

TEST_CASE("FortranEvaluator a program unit using an earlier cell's module") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    CHECK(e.evaluate2(R"(module mearly
implicit none
contains
subroutine say(i)
integer, intent(in) :: i
if (i /= 3) error stop
end subroutine
end module
)").ok);
    // A program unit loads the submodules of what it uses, and that walks the
    // dependencies of mlate. mearly is one of them and it is a parent scope
    // away, not a modfile on disk.
    const char *cell = R"(module mlate
use mearly
implicit none
contains
subroutine show()
call say(3)
end subroutine
end module
program p
use mlate
implicit none
call show()
end program
)";
    CHECK(e.evaluate2(cell).ok);
    CHECK(e.evaluate2(cell).ok);
}

TEST_CASE("FortranEvaluator a type-bound procedure from an earlier cell") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    // The copy of the type carries the binding, which names the procedure it
    // binds to. Left pointing at the original, the call reaches a procedure
    // this cell does not hold and code generation never declares.
    CHECK(e.evaluate2(R"(module mtb
implicit none
type :: point
    real :: x
contains
    procedure :: norm
end type
contains
real function norm(self)
class(point), intent(in) :: self
norm = abs(self%x)
end function
end module
)").ok);
    const char *caller = R"(program p
use mtb
implicit none
type(point) :: a
a%x = -2.0
if (a%norm() /= 2.0) error stop
end program
)";
#ifdef _WIN32
    // Running this hangs on Windows, and so does calling any procedure of an
    // earlier cell that takes a polymorphic argument -- a bug of its own, see
    // https://github.com/lfortran/lfortran/issues/12494, and not one this fix
    // introduced: before it the call did not compile at all. Compiling is what
    // this test is about: without the fix above it fails with "Function code
    // not generated for 'norm'".
    {
        LCompilers::LocationManager lm;
        LCompilers::PassManager lpm;
        lpm.use_default_passes();
        LCompilers::diag::Diagnostics d;
        CHECK(e.get_llvm(caller, lm, lpm, d).ok);
    }
#else
    CHECK(e.evaluate2(caller).ok);
#endif
}

TEST_CASE("FortranEvaluator a derived type argument from an earlier cell") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    // A function returning a derived type is turned into a subroutine by a
    // pass, and the JIT holds it in that form, so the copy has to be
    // transformed the same way. Its dummy arguments also name the type they
    // were declared with, which has to be this chain's copy of it or the call
    // does not type check.
    CHECK(e.evaluate2(R"(module mret
implicit none
type :: vec
    real :: x
end type
contains
type(vec) function addv(a, b)
type(vec), intent(in) :: a, b
addv%x = a%x + b%x
end function
end module
)").ok);
    CHECK(e.evaluate2(R"(program p
use mret
implicit none
type(vec) :: a, b, c
a%x = 1.0
b%x = 2.0
c = addv(a, b)
if (c%x /= 3.0) error stop
end program
)").ok);
}

TEST_CASE("FortranEvaluator a type re-exported by a module across cells") {
    CompilerOptions cu;
    cu.interactive = true;
    // Not get_runtime_library_dir(): this binary never sets the execution
    // mode that answer depends on, so it cannot find the modfiles itself.
    cu.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
    FortranEvaluator e(cu);
    // A module the frontend maps an intrinsic name onto holds its own entry
    // for `c_ptr` as an import rather than as the derived type. A later cell
    // relinks its copy of the module against that entry, which would name an
    // ExternalSymbol from an ExternalSymbol -- a chain nothing that reads the
    // symbol can follow.
    const char *cell = R"(module mcptr
use iso_c_binding, only: c_ptr
implicit none
contains
subroutine takes_ptr(p)
type(c_ptr), intent(in) :: p
end subroutine
end module
program p
use mcptr
use iso_c_binding, only: c_ptr, c_null_ptr
implicit none
type(c_ptr) :: q
q = c_null_ptr
call takes_ptr(q)
end program
)";
    CHECK(e.evaluate2(cell).ok);
    CHECK(e.evaluate2(cell).ok);
}

TEST_CASE("FortranEvaluator a cell declaring a requirement and a template") {
    CompilerOptions cu;
    cu.interactive = true;
    // Not get_runtime_library_dir(): this binary never sets the execution
    // mode that answer depends on, so it cannot find the modfiles itself.
    cu.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
    FortranEvaluator e(cu);
    // Every cell is snapshotted symbol by symbol through SymbolDuplicator so
    // the next one is parented to a tree no ASR pass has touched. A kind the
    // duplicator does not know about throws and kills the cell, which is what
    // a Requirement and a Template used to do.
    const char *cell = R"(module mtempl
implicit none
requirement r {t, op}
deferred type :: t
deferred interface
function op(x, y) result(z)
type(t), intent(in) :: x, y
type(t) :: z
end function
end interface
end requirement
template add_t {t, op}
require r {t, op}
contains
function add_generic(x, y) result(z)
type(t), intent(in) :: x, y
type(t) :: z
z = op(x, y)
end function
end template
end module
)";
    CHECK(e.evaluate2(cell).ok);
    // again, so the second cell is built on the duplicated scope
    CHECK(e.evaluate2(cell).ok);
}

TEST_CASE("FortranEvaluator re-run a cell declaring an operator") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    // A custom operator names its specific procedures the way a generic
    // interface does, and asking it for a scope it does not have used to
    // abort the copy outright.
    const char *cell = R"(module mop
implicit none
type :: vec
    real :: x
end type
interface operator(+)
    module procedure addv
end interface
contains
type(vec) function addv(a, b)
type(vec), intent(in) :: a, b
addv%x = a%x + b%x
end function
end module
program p
use mop
implicit none
type(vec) :: a, b, c
a%x = 1.0
b%x = 2.0
c = a + b
if (c%x /= 3.0) error stop
end program
)";
    CHECK(e.evaluate2(cell).ok);
    CHECK(e.evaluate2(cell).ok);
}

TEST_CASE("FortranEvaluator a diagnostic about an earlier cell") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    CHECK(e.evaluate2("module mdp; integer, parameter :: dp = kind(0.0d0); end module\n").ok);
    // The name is declared by the first cell, so the message has to quote that
    // cell. Each cell is compiled on its own; without a range of its own, the
    // location is read as a position in this cell and quotes its text.
    const char *cell = "use mdp; integer, parameter :: dp = kind(0.0)\n";
    // As the prompt and the kernel set it up: nothing, the evaluator names the
    // cell and keeps its text.
    LCompilers::LocationManager lm;
    LCompilers::PassManager lpm;
    lpm.use_default_passes();
    LCompilers::diag::Diagnostics d;
    LCompilers::Result<FortranEvaluator::EvalResult> r
        = e.evaluate(cell, false, lm, lpm, d);
    CHECK(!r.ok);
    cu.use_colors = false;
    std::string message = d.render(lm, cu);
    CHECK(message.find("use-associated from module 'mdp'") != std::string::npos);
    CHECK(message.find("<cell 1>") != std::string::npos);
    CHECK(message.find("<cell 2>") != std::string::npos);
    // The quoted line is the one that declares dp, not this cell's text.
    CHECK(message.find("module mdp; integer, parameter :: dp = kind(0.0d0)")
        != std::string::npos);
}

TEST_CASE("FortranEvaluator a module's derived type globals across cells") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    // The members of a derived type global that no static initializer can
    // describe are set up by executable code. That has to run once, when the
    // cell declaring the global runs: the cell below has no program, and a
    // later program must not set the global up again, which would reset
    // whatever the cells in between assigned to it.
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2(R"(module rm3
implicit none
type :: t
integer :: z = 7
integer, allocatable :: a(:)
end type
type :: u
integer :: z = 7
end type
type(t) :: arr(3)
type(t) :: s
type(u) :: su
integer :: seen = 0
end module
)");
    CHECK(r.ok);
    CHECK(e.evaluate2("use rm3\n").ok);
    r = e.evaluate2("arr(3)%z\n");
    CHECK(r.ok);
    CHECK(r.result.i32 == 7);
    CHECK(e.evaluate2("arr(1)%z = 99\n").ok);
    CHECK(e.evaluate2("s%z = 98\n").ok);
    CHECK(e.evaluate2("su%z = 97\n").ok);
    r = e.evaluate2("arr(1)%z\n");
    CHECK(r.ok);
    CHECK(r.result.i32 == 99);
    CHECK(e.evaluate2(R"(program p
use rm3
implicit none
seen = arr(1)%z
end program
)").ok);
    r = e.evaluate2("seen\n");
    CHECK(r.ok);
    CHECK(r.result.i32 == 99);
    r = e.evaluate2("arr(1)%z\n");
    CHECK(r.ok);
    CHECK(r.result.i32 == 99);
    r = e.evaluate2("s%z\n");
    CHECK(r.ok);
    CHECK(r.result.i32 == 98);
    r = e.evaluate2("su%z\n");
    CHECK(r.ok);
    CHECK(r.result.i32 == 97);
    r = e.evaluate2("arr(2)%z\n");
    CHECK(r.ok);
    CHECK(r.result.i32 == 7);
}

TEST_CASE("FortranEvaluator a module allocatable array across cells") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    // The descriptor of an allocatable module array has to outlive the
    // program cell that allocates it. A later cell, including another
    // program, sees that allocation and its size.
    FortranEvaluator e(cu);
    REQUIRE(e.evaluate2(R"(module m_alloc_arr_cell
implicit none
integer, allocatable :: x(:)
end module
program p_alloc_arr_cell
use m_alloc_arr_cell
implicit none
allocate(x(1))
x = 5
end program
)").ok);
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2(
        "use m_alloc_arr_cell\nallocated(x)\n");
    REQUIRE(r.ok);
    REQUIRE(r.result.type == FortranEvaluator::EvalResult::boolean);
    REQUIRE(r.result.b);
    r = e.evaluate2("size(x)\n");
    REQUIRE(r.ok);
    REQUIRE(r.result.i32 == 1);
    r = e.evaluate2("sum(x)\n");
    REQUIRE(r.ok);
    REQUIRE(r.result.i32 == 5);
    REQUIRE(e.evaluate2(R"(program q_alloc_arr_cell
use m_alloc_arr_cell
implicit none
if (.not. allocated(x)) error stop
if (size(x) /= 1) error stop
if (sum(x) /= 5) error stop
end program
)").ok);

    // The allocation is made by a later program than the module, then
    // observed by a statement cell and by another program. The size stays
    // the one allocate() set.
    FortranEvaluator e2(cu);
    REQUIRE(e2.evaluate2(R"(module m_alloc_arr_size
implicit none
integer, allocatable :: y(:)
end module
)").ok);
    REQUIRE(e2.evaluate2(R"(program p_alloc_arr_size
use m_alloc_arr_size
implicit none
allocate(y(3))
y = 5
end program
)").ok);
    r = e2.evaluate2("use m_alloc_arr_size\nallocated(y)\n");
    REQUIRE(r.ok);
    REQUIRE(r.result.type == FortranEvaluator::EvalResult::boolean);
    REQUIRE(r.result.b);
    r = e2.evaluate2("size(y)\n");
    REQUIRE(r.ok);
    REQUIRE(r.result.i32 == 3);
    r = e2.evaluate2("sum(y)\n");
    REQUIRE(r.ok);
    REQUIRE(r.result.i32 == 15);
    REQUIRE(e2.evaluate2(R"(program p_alloc_arr_size2
use m_alloc_arr_size
implicit none
if (.not. allocated(y)) error stop
if (size(y) /= 3) error stop
if (sum(y) /= 15) error stop
end program
)").ok);
    r = e2.evaluate2("size(y)\n");
    REQUIRE(r.ok);
    REQUIRE(r.result.i32 == 3);
    r = e2.evaluate2("sum(y)\n");
    REQUIRE(r.ok);
    REQUIRE(r.result.i32 == 15);
}

TEST_CASE("FortranEvaluator initialized bind(c) global across cells") {
    CompilerOptions cu;
    cu.interactive = true;
    SUBCASE("normal") {}
    SUBCASE("fast") { cu.po.fast = true; }
    SUBCASE("Fortran 2023") { cu.std_f23_conformance = true; }
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    REQUIRE(e.evaluate2(R"(module zb
integer, bind(c) :: gi = 7
end module
)").ok);
    REQUIRE(e.evaluate2("use zb\n").ok);
    auto r = e.evaluate2("gi\n");
    REQUIRE(r.ok);
    CHECK(r.result.i32 == 7);
    REQUIRE(e.evaluate2("gi = 19\n").ok);
    REQUIRE(e.evaluate2("1+1\n").ok);
    r = e.evaluate2("gi\n");
    REQUIRE(r.ok);
    CHECK(r.result.i32 == 19);
}

#ifndef __EMSCRIPTEN__
TEST_CASE("FortranEvaluator bind(c) threadprivate global across cells") {
    namespace ASR = LCompilers::ASR;
    CompilerOptions cu;
    cu.interactive = true;
    cu.openmp = true;
    SUBCASE("normal") {}
    SUBCASE("fast") { cu.po.fast = true; }
    SUBCASE("Fortran 2023") { cu.std_f23_conformance = true; }
    cu.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
    FortranEvaluator e(cu);
    REQUIRE(e.evaluate2(R"(module cell_tls
use iso_c_binding, only: c_int
integer(c_int), bind(c) :: cell_tls_counter = 7
!$omp threadprivate(cell_tls_counter)
contains
integer function read_counter()
    read_counter = cell_tls_counter
end function
end module
)").ok);
    REQUIRE(e.evaluate2("use cell_tls\n").ok);
    REQUIRE(e.evaluate2("1+1\n").ok);
    // ORC's TLS runtime is not available on every host. Check the actual
    // evaluator's retained ASR here; the two-file integration test executes TLS.
    LCompilers::LocationManager lm;
    LCompilers::diag::Diagnostics diagnostics;
    auto asr = e.get_asr2("cell_tls_counter = 19\n", lm, diagnostics);
    REQUIRE(asr.ok);
    auto *counter = ASR::down_cast<ASR::Variable_t>(
        LCompilers::ASRUtils::symbol_get_past_external(
            asr.result->m_symtab->resolve_symbol("cell_tls_counter")));
    CHECK(counter->m_storage == ASR::storage_typeType::Threadprivate);
    CHECK(counter->m_abi == ASR::abiType::BindC);
    CHECK(counter->m_external_storage);
}
#endif

TEST_CASE("FortranEvaluator bind(c) global storage persists across cells") {
    CompilerOptions cu;
    cu.interactive = true;
    SUBCASE("normal") {}
    SUBCASE("fast") { cu.po.fast = true; }
    SUBCASE("Fortran 2023") { cu.std_f23_conformance = true; }
    cu.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
    FortranEvaluator e(cu);
    REQUIRE(e.evaluate2(R"(module zb2
use iso_c_binding
implicit none
type, bind(c) :: t
    integer(c_int) :: z
end type
type(t), bind(c) :: gx = t(7)
integer(c_int), bind(c, name="cell_named") :: named = 8
integer(c_int), bind(c, name="") :: unnamed = 9
integer(c_int), bind(c) :: uninitialized
integer(c_int), bind(c), target :: target_value = 42
type(c_ptr), bind(c) :: gp = c_null_ptr
integer :: regular = 11
real(c_double), bind(c) :: gr = 1.5
complex(c_double_complex), bind(c) :: gz = (2.0, 3.0)
logical(c_bool), bind(c) :: gl = .true.
character(c_char), bind(c) :: gc = 'a'
integer(c_int), bind(c) :: ga(2) = [4, 5]
character(c_char), bind(c) :: gca(2) = ['b', 'c']
end module
)").ok);
    REQUIRE(e.evaluate2("use zb2\n").ok);
    REQUIRE(e.evaluate2(R"(if (gx%z /= 7 .or. named /= 8 .or. unnamed /= 9) error stop
if (gr /= 1.5 .or. gz /= (2.0, 3.0) .or. .not. gl) error stop
if (gc /= 'a' .or. any(ga /= [4, 5]) .or. any(gca /= ['b', 'c'])) error stop
if (c_associated(gp) .or. regular /= 11) error stop
gx%z = 17
named = 18
unnamed = 19
uninitialized = 20
gr = 2.5
gz = (3.0, 4.0)
gl = .false.
gc = 'z'
ga = [6, 7]
gca = ['x', 'y']
gp = c_loc(target_value)
regular = 21
)").ok);
    REQUIRE(e.evaluate2(R"(program p
use zb2
implicit none
if (gx%z /= 17 .or. named /= 18 .or. unnamed /= 19) error stop
if (uninitialized /= 20 .or. gr /= 2.5 .or. gz /= (3.0, 4.0) .or. gl) error stop
if (gc /= 'z' .or. any(ga /= [6, 7]) .or. any(gca /= ['x', 'y'])) error stop
if (.not. c_associated(gp, c_loc(target_value)) .or. regular /= 21) error stop
end program
)").ok);
    auto r = e.evaluate2("gx%z + uninitialized\n");
    REQUIRE(r.ok);
    CHECK(r.result.i32 == 37);
}

TEST_CASE("FortranEvaluator the calls the kernel makes") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    // The kernel reaches the evaluator through get_ast(), get_asr() and
    // get_llvm() as well, for its %%showast, %%showasr and %%showllvm magics --
    // Demo1.ipynb ends on three such cells. Each is a cell of its own, and a
    // cell that is never opened cannot be closed.
    auto kernel_lm = []() { return LCompilers::LocationManager(); };
    {
        LCompilers::LocationManager lm = kernel_lm();
        LCompilers::PassManager lpm;
        lpm.use_default_passes();
        LCompilers::diag::Diagnostics d;
        CHECK(e.evaluate("1+3", false, lm, lpm, d).ok);
        CHECK(lm.file_ends.size() == 1);
    }
    {
        LCompilers::LocationManager lm = kernel_lm();
        LCompilers::diag::Diagnostics d;
        CHECK(e.get_ast("integer :: i\n", lm, d).ok);
        CHECK(lm.file_ends.size() == 2);
    }
    {
        LCompilers::LocationManager lm = kernel_lm();
        LCompilers::diag::Diagnostics d;
        CHECK(e.get_asr("integer :: j\n", lm, d).ok);
        CHECK(lm.file_ends.size() == 3);
    }
    {
        LCompilers::LocationManager lm = kernel_lm();
        LCompilers::PassManager lpm;
        lpm.use_default_passes();
        LCompilers::diag::Diagnostics d;
        CHECK(e.get_llvm("integer :: k\n", lm, lpm, d).ok);
        CHECK(lm.file_ends.size() == 4);
    }
    {
        // A file compiled in interactive mode -- which is how a file holding
        // statements outside any program unit is compiled -- keeps its name.
        LCompilers::LocationManager lm;
        LCompilers::LocationManager::FileLocations fl;
        fl.in_filename = "some_file.f90";
        lm.files.push_back(fl);
        LCompilers::diag::Diagnostics d;
        CHECK(e.get_ast("integer :: m\n", lm, d).ok);
        CHECK(lm.files.back().in_filename == "some_file.f90");
    }
}

// These two tests hang on Windows since
// https://github.com/lfortran/lfortran/pull/14350
#if !defined(_WIN32)
TEST_CASE("FortranEvaluator a SAVE class pointer across cells") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    // Each cell is a separate LLVM module. Every one of them that associates
    // or disassociates `gp` has to use the one static class wrapper of `gp`.
    CHECK(e.evaluate2(R"(module mcp1
implicit none
type :: s
    integer :: i = 0
end type
type(s), target :: x
end module
)").ok);
    CHECK(e.evaluate2("use mcp1").ok);
    CHECK(e.evaluate2("class(s), pointer :: gp => null()").ok);
    CHECK(e.evaluate2("class(s), pointer, save :: gq").ok);
    CHECK(e.evaluate2("x%i = 4").ok);
    for (int k = 0; k < 2; k++) {
        CHECK(e.evaluate2("gp => x").ok);
        CHECK(e.evaluate2("gq => x").ok);
        LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2("gp%i + gq%i");
        CHECK(r.ok);
        CHECK(r.result.i32 == 8);
        CHECK(e.evaluate2("gp => null()").ok);
        // A top-level `nullify(gq)` is taken for a function call (#13455).
        CHECK(e.evaluate2("if (associated(gq)) nullify(gq)").ok);
        r = e.evaluate2("associated(gp) .or. associated(gq)");
        CHECK(r.ok);
        CHECK(r.result.b == false);
        CHECK(e.evaluate2("if (.not. associated(gp)) allocate(gp)").ok);
        CHECK(e.evaluate2("gp%i = 9").ok);
        r = e.evaluate2("gp%i");
        CHECK(r.ok);
        CHECK(r.result.i32 == 9);
        CHECK(e.evaluate2("if (associated(gp)) deallocate(gp)").ok);
        r = e.evaluate2("associated(gp)");
        CHECK(r.ok);
        CHECK(r.result.b == false);
    }
}

TEST_CASE("FortranEvaluator a SAVE class pointer of a redefined module") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    // The old and the new module `mcp2` each keep their own `p`, and so their
    // own static class wrapper of it.
    auto module_mcp2 = [](int i) {
        return R"(module mcp2
implicit none
type :: s
    integer :: i = 0
end type
class(s), pointer :: p => null()
type(s), target :: x
contains
subroutine set()
    x%i = )" + std::to_string(i) + R"(
    p => x
end subroutine
integer function get()
    get = -1
    if (associated(p)) get = p%i
end function
end module
)";
    };
    CHECK(e.evaluate2(module_mcp2(1)).ok);
    CHECK(e.evaluate2(R"(integer function old_get()
use mcp2, only: get
old_get = get()
end function
)").ok);
    CHECK(e.evaluate2(R"(subroutine old_set()
use mcp2, only: set
call set()
end subroutine
)").ok);
    CHECK(e.evaluate2(module_mcp2(2)).ok);
    CHECK(e.evaluate2(R"(integer function new_get()
use mcp2, only: get
new_get = get()
end function
)").ok);
    CHECK(e.evaluate2(R"(subroutine new_set()
use mcp2, only: set
call set()
end subroutine
)").ok);
    CHECK(e.evaluate2("call old_set()").ok);
    CHECK(e.evaluate2("call new_set()").ok);
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2("old_get()");
    CHECK(r.ok);
    CHECK(r.result.i32 == 1);
    r = e.evaluate2("new_get()");
    CHECK(r.ok);
    CHECK(r.result.i32 == 2);
}
#endif

extern "C" void lfortran_display_data(const char *mime_type, const char *data);
extern "C" void lfortran_clear_output();

static CompilerOptions execute_options() {
    CompilerOptions cu;
    cu.interactive = true;
    cu.use_colors = false;
    // Cells that use runtime modules need their modfiles.
    cu.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
    return cu;
}

TEST_CASE("FortranEvaluator execute: results and cells") {
    CompilerOptions cu = execute_options();
    FortranEvaluator e(cu);
    const std::vector<std::pair<std::string, std::string>> examples = {
        {"42", "42"}, {"42_8", "42"}, {"1.25", "1.25"},
        {"1.2345678901234567_8", "1.2345678901234567"},
        {"(1.25, -2.5)", "(1.25, -2.5)"},
        {"(1.25_8, -2.5_8)", "(1.25, -2.5)"},
        // A real always has a decimal point or an exponent.
        {"1.0", "1.0"}, {"-3.0_8", "-3.0"}, {"(1.0, -2.0)", "(1.0, -2.0)"},
        {"(0.0_8, 4.0_8)", "(0.0, 4.0)"}, {"1.0e20", "1e+20"},
        // The shortest text that reads back as the same value.
        {"3.14159", "3.14159"}, {"0.1_8", "0.1"}, {"1e-5", "1e-05"},
        {"1.0/3.0", "0.33333334"}, {"tiny(1.0)", "1.1754944e-38"},
        {".true.", "True"}, {".false.", "False"}, {"'hello'", "hello"}
    };
    for (auto &example : examples) {
        CAPTURE(example.first);
        auto cell = e.execute(example.first);
        REQUIRE(cell.ok);
        CHECK(!cell.inspection);
        CHECK(cell.value.has_value());
        CHECK(cell.value.text() == example.second);
    }
    auto cell = e.execute("integer :: n\nn = 41");
    REQUIRE(cell.ok);
    CHECK(cell.diagnostics.empty());
    CHECK(!cell.value.has_value());
    CHECK(e.execute("n+1").value.text() == "42");
    cell = e.execute("");
    REQUIRE(cell.ok);
    CHECK(!cell.value.has_value());
    cell = e.execute("integer :: broken(");
    CHECK(!cell.ok);
    CHECK(cell.error_name == "CompilerError");
    CHECK(!cell.internal_error);
    CHECK(cell.error_value.find("<cell ") != std::string::npos);
    CHECK(cell.error_text() == cell.error_value);
    CHECK(e.execute("n").value.text() == "41");
    // An internal compiler error keeps its message apart from the stack trace.
    // Array results are not supported yet (#13456).
    cell = e.execute("[1, 2, 3]");
    CHECK(!cell.ok);
    CHECK(cell.internal_error);
    CHECK(cell.error_value == "Return type not supported in interactive mode");
    CHECK(cell.error_text().rfind("Internal Compiler Error: Unhandled exception\n", 0)
        == 0);
    CHECK(e.execute("n").value.text() == "41");
    // An inspection cell sees the session.
    cell = e.execute("%%showasr\nn + 1");
    CHECK(cell.ok);
    // A successful cell keeps its warnings.
    cell = e.execute("max0(1, 2_8)");
    REQUIRE(cell.ok);
    CHECK(cell.value.text() == "2");
    CHECK(cell.diagnostics.find("warning: Different kinds of args in max0")
        != std::string::npos);
}

TEST_CASE("FortranEvaluator execute: persistent implicit none") {
    for (int mode = 0; mode < 3; mode++) {
        for (bool implicit_typing : {false, true}) {
            CAPTURE(mode);
            CAPTURE(implicit_typing);
            CompilerOptions cu = execute_options();
            cu.implicit_typing = implicit_typing;
            cu.po.fast = mode == 1;
            cu.std_f23_conformance = mode == 2;
            FortranEvaluator e(cu);
            REQUIRE(e.execute("integer :: previous = 7").ok);
            auto cell = e.execute("implicit none");
            INFO(cell.error_text());
            REQUIRE(cell.ok);
            CHECK(!cell.value.has_value());
            CHECK(e.execute("previous").value.text() == "7");
            for (const std::string source : {"n = 1", "x = 1.0"}) {
                cell = e.execute(source);
                CHECK(!cell.ok);
                CHECK(!cell.internal_error);
                CHECK(cell.error_name == "CompilerError");
                CHECK(cell.diagnostics.find("not declared") != std::string::npos);
            }
            REQUIRE(e.execute("integer :: n").ok);
            REQUIRE(e.execute("n = 1").ok);
            cell = e.execute("n");
            REQUIRE(cell.ok);
            CHECK(cell.value.type == FortranEvaluator::EvalResult::integer4);
            CHECK(cell.value.i32 == 1);
            REQUIRE(e.execute("").ok);
            REQUIRE(e.execute("implicit none").ok);
            REQUIRE(e.execute("implicit none(type)").ok);
            CHECK(!e.execute("j = 2").ok);
            CHECK(!e.execute("y = 2.0").ok);
            CHECK(e.execute("n").value.text() == "1");
        }
    }
}

TEST_CASE("FortranEvaluator execute: implicit none scope and inspection") {
    CompilerOptions cu = execute_options();
    SUBCASE("normal") {}
    SUBCASE("fast") { cu.po.fast = true; }
    SUBCASE("f23") { cu.std_f23_conformance = true; }
    cu.implicit_typing = true;
    FortranEvaluator e(cu);
    for (const std::string magic : {"asr", "llvm", "asm", "cpp"}) {
#ifdef __EMSCRIPTEN__
        if (magic == "asm") continue;
#endif
        CAPTURE(magic);
        auto cell = e.execute("%%show" + magic + "\nimplicit none");
        INFO(cell.error_text());
        REQUIRE(cell.ok);
        CHECK(cell.inspection);
        REQUIRE(e.execute("n = 1").ok);
    }
    // A failed cell must not change the session's typing policy.
    auto cell = e.execute("implicit none\nmissing = 1");
    CHECK(!cell.ok);
    CHECK(!cell.internal_error);
    REQUIRE(e.execute("x = 2.0").ok);
    cell = e.execute("implicit none(external)");
    CHECK(!cell.ok);
    CHECK(!cell.internal_error);
    CHECK(cell.diagnostics.find("not supported") != std::string::npos);
    REQUIRE(e.execute("y = 2.0").ok);
    REQUIRE(e.execute("module local_rules\nimplicit none\nend module").ok);
    REQUIRE(e.execute("j = 3").ok);
    REQUIRE(e.execute("implicit none").ok);
    // A procedure's local implicit rules must not re-enable cell typing.
    REQUIRE(e.execute("integer function f()\nimplicit integer(a-z)\nf = 4\nend function").ok);
    CHECK(e.execute("f()").value.text() == "4");
    CHECK(!e.execute("k = 4").ok);
    // A fresh evaluator, even sharing compiler options, starts independently.
    FortranEvaluator other(cu);
    REQUIRE(other.execute("n = 5").ok);
    CHECK(other.execute("n").value.text() == "5");
    CHECK(!e.execute("z = 5.0").ok);
}

TEST_CASE("FortranEvaluator execute: inspection cells") {
    for (const std::string magic : {"ast", "asr", "llvm", "asm", "cpp", "fmt"}) {
#ifdef __EMSCRIPTEN__
        // In the WASM test build, the compilation after get_asm() traps (#15853).
        if (magic == "asm") continue;
#endif
        CAPTURE(magic);
        CompilerOptions cu = execute_options();
        FortranEvaluator e(cu);
        auto cell = e.execute("%%show" + magic + "\ninteger :: x");
        REQUIRE(cell.ok);
        CHECK(cell.inspection);
        CHECK(!cell.output.empty());
        CHECK(!cell.value.has_value());
        // Trailing blanks and a CRLF line ending after the magic.
        cell = e.execute("%%show" + magic + " \t\r\ninteger :: y");
        REQUIRE(cell.ok);
        CHECK(cell.inspection);
        CHECK(!cell.output.empty());
        // An inspection cell does not run, so its declarations are not part
        // of the session.
        cell = e.execute("x");
        CHECK(!cell.ok);
        CHECK(cell.error_name == "CompilerError");
    }
    FortranEvaluator::CellResult defaults;
    CHECK(!defaults.value.has_value());
}

TEST_CASE("FortranEvaluator execute: display hooks") {
    CompilerOptions cu = execute_options();
    FortranEvaluator e(cu);
    std::string mime, data;
    int clears = 0;
    FortranEvaluator::CellIO io;
    io.display = [&](const std::string &m, const std::string &d) { mime = m; data = d; };
    io.clear_output = [&]() { clears++; };
    auto cell = e.execute("use lfortran_display, only: display_data, clear_output\n"
        "call display_data('text/html', '<b>hello</b>')\n"
        "call clear_output()", false, io);
    REQUIRE(cell.ok);
    CHECK(mime == "text/html");
    CHECK(data == "<b>hello</b>");
    CHECK(clears == 1);
    // The hooks only receive calls made while their cell runs.
    lfortran_display_data("text/plain", "outside execution");
    lfortran_clear_output();
    CHECK(mime == "text/html");
    CHECK(data == "<b>hello</b>");
    CHECK(clears == 1);
    // A hook's exception must not unwind through generated code.
    io.clear_output = []() { throw std::runtime_error("display failure"); };
    cell = e.execute("call clear_output()", false, io);
    CHECK(!cell.ok);
    CHECK(cell.error_name == "DisplayError");
    CHECK(cell.error_value == "display failed: display failure");
    // The session goes on, with the hooks of the next cell.
    cell = e.execute("call display_data('text/plain', 'again')", false, io);
    REQUIRE(cell.ok);
    CHECK(data == "again");
}

// Like test_runtime_input, the stdin cases run natively only; the WASM test
// build also cannot read the integration test sources.
#ifndef __EMSCRIPTEN__
struct InputRecords {
    std::vector<std::string> records;
    size_t next = 0;
    int calls = 0;
    int end = -1;
    FortranEvaluator::CellIO io() {
        FortranEvaluator::CellIO io;
        io.input = [this](std::string &record) {
            calls++;
            if (next == records.size()) return end;
            record = records[next++];
            return 0;
        };
        return io;
    }
};

static void run_cell(FortranEvaluator &e, const std::string &code,
        const FortranEvaluator::CellIO &io = {}) {
    auto cell = e.execute(code, false, io);
    CAPTURE(code);
    REQUIRE_MESSAGE(cell.ok, cell.error_text());
}

static void check_input(const std::string &code,
        std::vector<std::string> records, int expected_calls = -1) {
    CompilerOptions cu = execute_options();
    FortranEvaluator e(cu);
    InputRecords input{std::move(records)};
    run_cell(e, code, input.io());
    CHECK(input.calls ==
        (expected_calls < 0 ? (int)input.records.size() : expected_calls));
}

TEST_CASE("FortranEvaluator execute: stdin records") {
    check_input(R"(
integer :: a, b, c, u, ios
u = 5
read(*,*,iostat=ios) a,b
if (ios /= 0 .or. a /= 11 .or. b /= 12) error stop
read(u,*,iostat=ios) c
if (ios /= 0 .or. c /= 13) error stop
read(*,*)
read(5,*,iostat=ios) a
if (ios /= 0 .or. a /= 14) error stop
)", {"11 12 ignored", "13", "", "14"});
    check_input(R"(
integer :: a(3), ios
real :: r(2)
complex :: c(2), z
complex(8) :: d(2)
read(*,*,iostat=ios) a
if (ios /= 0 .or. any(a /= [1,2,3])) error stop
read(5,*,iostat=ios) r
if (ios /= 0 .or. any(abs(r-[1.5,2.5]) > 1e-6)) error stop
read(*,*,iostat=ios) c
if (ios /= 0 .or. any(abs(c-[(1.,2.),(3.,4.)]) > 1e-6)) error stop
read(5,*,iostat=ios) d
if (ios /= 0 .or. any(abs(d-[(5.,6.),(7.,8.)]) > 1e-6)) error stop
read(*,*,iostat=ios) z
if (ios /= 0 .or. abs(z-(9.,10.)) > 1e-6) error stop
)", {"1,2", "3", "1.5 2.5", "(1,2), ( 3, 4 )", "(5,6) (7,8)", "(9,10)"});
    check_input(R"(
character(4) :: a
character(2) :: b
integer :: i,j,k,ios
read(*,'(a4)',iostat=ios) a
if (ios /= 0 .or. a /= '    ') error stop
read(5,'(a4)',iostat=ios) a
if (ios /= 0 .or. a /= 'text') error stop
read(*,'(i2,/,i2)',iostat=ios) i,j
if (ios /= 0 .or. i /= 21 .or. j /= 22) error stop
read(5,'(i2)',iostat=ios) i,j,k
if (ios /= 0 .or. i /= 31 .or. j /= 32 .or. k /= 33) error stop
read(5,'(a2)',advance='no',iostat=ios) b
if (ios /= 0 .or. b /= 'ab') error stop
read(*,'(a2)',iostat=ios) b
if (ios /= 0 .or. b /= 'cd') error stop
)", {"", "text", "21", "22", "31", "32", "33", "abcd"});
    check_input(R"(
program input_namelist
integer :: a,ios
namelist /values/ a
read(5,nml=values,iostat=ios)
if (ios /= 0 .or. a /= 57) error stop
read(*,nml=values,iostat=ios)
if (ios /= 0 .or. a /= 58) error stop
end program
)", {"&values", "a=57 /", "&values a=58 /"});
    check_input(R"(
integer(1) :: a(2)
integer(2) :: b(2)
integer(8) :: c(2)
real :: r(2)
real(8) :: d(2)
logical :: l(2)
character(3) :: t(2)
integer :: ios
read(*,*,iostat=ios) a
if (ios /= 0 .or. any(a /= [1,2])) error stop
read(5,*,iostat=ios) b
if (ios /= 0 .or. any(b /= [3,4])) error stop
read(*,*,iostat=ios) c
if (ios /= 0 .or. any(c /= [5,6])) error stop
read(*,*,iostat=ios) r
if (ios /= 0 .or. any(abs(r-[1.5,2.5]) > 1e-6)) error stop
read(*,*,iostat=ios) d
if (ios /= 0 .or. any(abs(d-[3.5,4.5]) > 1e-6)) error stop
read(*,*,iostat=ios) l
if (ios /= 0 .or. .not. l(1) .or. l(2)) error stop
read(*,*,iostat=ios) t
if (ios /= 0 .or. t(1) /= 'abc' .or. t(2) /= 'def') error stop
)", {"1 2", "3 4", "5 6", "1.5,2.5", "3.5,4.5", "T F", "'abc' 'def'"});
    check_input(R"(
character(2000) :: text
integer :: ios
read(5,'(a2000)',advance='no',iostat=ios) text
if (ios /= 0 .or. text /= repeat('x',2000)) error stop
read(*,'(a2000)',iostat=ios) text
if (ios /= 0 .or. text /= repeat('x',2000)) error stop
)", {std::string(4000, 'x')});
    check_input(R"(
character(2) :: a,b,c
integer :: ios
read(5,'(a2,t1,a2,tl1,a2)',iostat=ios) a,b,c
if (ios /= 0 .or. a /= 'ab' .or. b /= 'ab' .or. c /= 'bc') error stop
read(*,'(t2,a2)',iostat=ios) a
if (ios /= 0 .or. a /= 'bc') error stop
read(5,'(tl1,a2)',iostat=ios) a
if (ios /= 0 .or. a /= 'ab') error stop
)", {"abcd", "abcd", "abcd"});
}

TEST_CASE("FortranEvaluator execute: stdin EOF and reset") {
    check_input(R"(
program input_eof
integer :: a(2),ios
complex :: z
character(3) :: text
namelist /values/ a
read(*,'(a3)',iostat=ios) text
if (ios /= -1) error stop 'formatted EOF'
read(5,*,iostat=ios) a
if (ios /= -1) error stop 'array EOF'
read(*,*,iostat=ios) z
if (ios /= -1) error stop 'complex EOF'
read(5,nml=values,iostat=ios)
if (ios /= -1) error stop 'namelist EOF'
read(*,*,iostat=ios)
if (ios /= -1) error stop 'empty read EOF'
end program
)", {}, 1);
    CompilerOptions cu = execute_options();
    FortranEvaluator e(cu);
    // EOF stays latched for the rest of the cell.
    InputRecords eof;
    run_cell(e, R"(
integer :: a,ios
read(*,*,iostat=ios) a
if (ios /= -1) error stop
read(5,*,iostat=ios) a
if (ios /= -1) error stop
)", eof.io());
    CHECK(eof.calls == 1);
    // A later cell can read again.
    InputRecords fresh{{"42"}};
    run_cell(e, "read(5,*,iostat=ios) a\nif (ios /= 0 .or. a /= 42) error stop",
        fresh.io());
    // Unavailable input is a positive IOSTAT, also latched.
    InputRecords disconnected;
    disconnected.end = 1;
    run_cell(e, R"(
read(*,*,iostat=ios) a
if (ios <= 0) error stop
read(5,'(i2)',iostat=ios) a
if (ios <= 0) error stop
)", disconnected.io());
    CHECK(disconnected.calls == 1);
    // A hook's exception must not unwind through generated code.
    FortranEvaluator::CellIO throwing;
    throwing.input = [](std::string &) -> int {
        throw std::runtime_error("input callback failure");
    };
    run_cell(e, "read(*,*,iostat=ios) a\nif (ios <= 0) error stop", throwing);
    run_cell(e, "a+1");
}

TEST_CASE("FortranEvaluator execute: files never read stdin records") {
    CompilerOptions cu = execute_options();
    FortranEvaluator e(cu);
    InputRecords forbidden;
    forbidden.end = 1;
    std::ifstream file(std::string(LFORTRAN_PROJECT_SOURCE_DIR) +
        "/integration_tests/runtime_input_records.f90");
    REQUIRE(file.good());
    std::ostringstream source;
    source << file.rdbuf();
    run_cell(e, source.str(), forbidden.io());
    run_cell(e, R"(
program unit5_named_file
integer :: u,i,ios
character(3) :: r
u=5
open(unit=u,file='test_execute_unit5.dat',status='replace')
write(u,'(a)') '123'
rewind(u)
read(u,*,iostat=ios) i
if (ios /= 0 .or. i /= 123) error stop
close(u,status='delete')
r='456'
read(r,*,iostat=ios) i
if (ios /= 0 .or. i /= 456) error stop
end program
)", forbidden.io());
    CHECK(forbidden.calls == 0);
}
#endif
