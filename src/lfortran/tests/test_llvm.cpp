#include <tests/doctest.h>

#include <cmath>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <fstream>
#include <functional>
#include <map>
#include <set>
#include <vector>

#include <lfortran/fortran_evaluator.h>
#include <lfortran/fortran_kernel.h>
#ifndef __EMSCRIPTEN__
#include <thread>
#ifndef _WIN32
#include <pthread.h>
#include <sys/resource.h>
#endif
#include <libasr/codegen/KaleidoscopeJIT.h>
#if defined(__linux__)
#include <sys/mman.h>
#include <unistd.h>
#endif
#endif
#include <lfortran/parser/source_offset.h>
#include <libasr/codegen/evaluator.h>
#include <libasr/exception.h>
#include <lfortran/ast.h>
#include <libasr/asr.h>
#include <lfortran/parser/parser.h>
#include <lfortran/semantics/ast_to_asr.h>
#include <libasr/codegen/asr_to_llvm.h>
#include <libasr/codegen/llvm_reductions.h>
#include <libasr/codegen/llvm_rsqrt.h>
#include <lfortran/pickle.h>
#include <libasr/pickle.h>
#include <libasr/modfile.h>
#include <libasr/utils.h>
#include <lfortran/utils.h>

#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Config/llvm-config.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GetElementPtrTypeIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Operator.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#if LLVM_VERSION_MAJOR >= 14
#include <llvm/MC/TargetRegistry.h>
#else
#include <llvm/Support/TargetRegistry.h>
#endif
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

TEST_CASE("JIT code sections share one reservation") {
    llvm::orc::ContiguousSectionMemoryManager memory;
#if LLVM_VERSION_MAJOR >= 16
    const llvm::Align alignment(64);
#else
    const uint32_t alignment = 64;
#endif
    memory.reserveAllocationSpace(65536 + 128, alignment,
        131072, alignment, 0, alignment);
    uint8_t *first = memory.allocateCodeSection(33, 32, 0, ".text.first");
    REQUIRE(first != nullptr);
    REQUIRE(memory.allocateDataSection(131072, 64, 1, ".rodata", true)
        != nullptr);
    uint8_t *second = memory.allocateCodeSection(65536, 64, 2, ".text.second");
    REQUIRE(second != nullptr);
    uint8_t *third = memory.allocateCodeSection(8, 16, 3, ".text.third");
    REQUIRE(third != nullptr);
    CHECK(second == first + 64);
    CHECK(third == second + 65536);
    CHECK(reinterpret_cast<uintptr_t>(second) % 64 == 0);
    std::string error;
    INFO(error);
    CHECK_FALSE(memory.finalizeMemory(&error));
}

TEST_CASE("JIT code that overflows its reservation is mapped separately") {
    llvm::orc::ContiguousSectionMemoryManager memory;
#if LLVM_VERSION_MAJOR >= 16
    const llvm::Align alignment(16);
#else
    const uint32_t alignment = 16;
#endif
    memory.reserveAllocationSpace(64, alignment, 0, alignment, 0, alignment);
    uint8_t *first = memory.allocateCodeSection(40, 16, 0, ".text.first");
    REQUIRE(first != nullptr);
    // Aligned to 48, 32 more bytes would end 16 bytes past the reservation.
    uint8_t *second = memory.allocateCodeSection(32, 16, 1, ".text.second");
    REQUIRE(second != nullptr);
    CHECK((second + 32 <= first || second >= first + 64));
    // The reservation keeps its offset, so a section that fits still uses it.
    uint8_t *third = memory.allocateCodeSection(16, 16, 2, ".text.third");
    CHECK(third == first + 48);
    std::string error;
    INFO(error);
    CHECK_FALSE(memory.finalizeMemory(&error));
}

#if defined(__linux__)
// A COMDAT function gets its own ELF code section, as do the procedures of an
// intrinsic module. The GNU unwinder registers the whole min/max PC range of
// an object's .eh_frame, so the object's code sections must stay together:
// another object's code placed between them corrupts libgcc's frame registry.
TEST_CASE("JIT object code stays in one reservation") {
    auto stores = [](int count) {
        std::string body;
        for (int i = 0; i < count; i++) {
            body += "    store volatile i64 " + std::to_string(i)
                + ", i64* @sink\n";
        }
        return body;
    };
    // The first code section takes several pages; the second does not fit
    // in what is left of them.
    const std::string main_body = stores(4096), side_body = stores(1024);
    const size_t page = sysconf(_SC_PAGESIZE);
    const size_t hole = 8 * page, spacer = 1 << 20;
    for (int i = 0; i < 8; i++) {
        CAPTURE(i);
        // Leave a free hole above a 1 MiB spacer. A code section that is
        // mapped on its own may land in the hole, far from the first one.
        char *region = static_cast<char *>(mmap(nullptr, spacer + hole + page,
            PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0));
        REQUIRE(region != MAP_FAILED);
        REQUIRE(munmap(region + spacer, hole) == 0);
        {
            const std::string n = std::to_string(i);
            LCompilers::LLVMEvaluator e;
            e.add_module("@sink = global i64 0\n"
                "$side" + n + " = comdat any\n"
                "define void @main" + n + "() {\n" + main_body
                + "    ret void\n}\n"
                "define i64 @side" + n + "() comdat {\n" + side_body
                + "    ret i64 " + n + "\n}\n");
            const intptr_t main_code = e.get_symbol_address("main" + n);
            const intptr_t side_code = e.get_symbol_address("side" + n);
            const intptr_t distance = side_code > main_code
                ? side_code - main_code : main_code - side_code;
            CHECK(distance < 256 * 1024);
            CHECK(e.execfn<int64_t>("side" + n) == i);
        }
        munmap(region, spacer);
        munmap(region + spacer + hole, page);
    }
}
#endif

TEST_CASE("LLVM target configuration") {
    CompilerOptions default_options;
    LCompilers::LLVMTargetConfig default_config
        = LCompilers::resolve_llvm_target_config(default_options);
    // LLVM 7-9 give real(16) arguments a register class only when the x86-64
    // subtarget has MMX, so the default CPU is the x86-64 baseline rather
    // than "generic" (see resolve_llvm_target_config in evaluator.cpp).
    std::string default_cpu = "generic";
#if LLVM_VERSION_MAJOR < 10
    if (llvm::Triple(default_config.triple).getArch()
            == llvm::Triple::x86_64) {
        default_cpu = "x86-64";
    }
#endif
    CHECK(default_config.cpu == default_cpu);
    CHECK(!default_config.emit_cpu_attribute);
    CHECK(default_config.features.empty());

    CompilerOptions fast_options;
    fast_options.po.fast = true;
    LCompilers::LLVMTargetConfig fast_config
        = LCompilers::resolve_llvm_target_config(fast_options);
    CHECK(fast_config.host_target);
    CHECK(fast_config.emit_cpu_attribute);
    CHECK(!fast_config.cpu.empty());

#ifdef __APPLE__
    // A deployment-target rewrite changes darwin to macosx. --fast still
    // targets this machine, so native CPU selection must keep working.
    const char *saved_deployment = std::getenv("MACOSX_DEPLOYMENT_TARGET");
    std::string saved_deployment_value = saved_deployment
        ? saved_deployment : "";
    setenv("MACOSX_DEPLOYMENT_TARGET", "11.0", 1);
    CompilerOptions deployed_options;
    deployed_options.po.fast = true;
    LCompilers::LLVMTargetConfig deployed_config
        = LCompilers::resolve_llvm_target_config(deployed_options);
    CHECK(deployed_config.host_target);
    CHECK(deployed_config.triple.find("macosx11.0.0") != std::string::npos);
    CHECK(!deployed_config.cpu.empty());
    if (saved_deployment) {
        setenv("MACOSX_DEPLOYMENT_TARGET", saved_deployment_value.c_str(), 1);
    } else {
        unsetenv("MACOSX_DEPLOYMENT_TARGET");
    }
#endif

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
    CHECK(tune_config.cpu == default_cpu);
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

TEST_CASE("LLVM --fast fuses multiplies and adds in the backend") {
    // Like -ffast-math (-ffp-contract=fast) in clang and flang.
    CompilerOptions default_options;
    LCompilers::LLVMEvaluator default_evaluator(default_options);
    CHECK(default_evaluator.get_target_machine().Options.AllowFPOpFusion
        == llvm::FPOpFusion::Standard);

    CompilerOptions fast_options;
    fast_options.po.fast = true;
    LCompilers::LLVMEvaluator fast_evaluator(fast_options);
    CHECK(fast_evaluator.get_target_machine().Options.AllowFPOpFusion
        == llvm::FPOpFusion::Fast);

    CompilerOptions strict_options;
    strict_options.po.fast = true;
    strict_options.po.no_fast_math = true;
    LCompilers::LLVMEvaluator strict_evaluator(strict_options);
    CHECK(strict_evaluator.get_target_machine().Options.AllowFPOpFusion
        == llvm::FPOpFusion::Standard);

#ifdef HAVE_TARGET_AARCH64
    // The AArch64 backend of LLVM 11 keeps `fmul fast` and `fadd fast`
    // separate unless the target machine allows fusion.
    CompilerOptions aarch64_options;
    aarch64_options.po.fast = true;
    aarch64_options.target = "aarch64-unknown-linux-gnu";
    LCompilers::LLVMEvaluator aarch64_evaluator(aarch64_options);
    std::unique_ptr<LCompilers::LLVMModule> m = aarch64_evaluator.parse_module2(R"""(
define double @f(double %x, double %y, double %z) {
    %m = fmul fast double %x, %y
    %s = fadd fast double %m, %z
    ret double %s
}
)""", "");
    std::string assembly = aarch64_evaluator.get_asm(*m->m_m);
    CAPTURE(assembly);
    CHECK(assembly.find("fmadd") != std::string::npos);
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
    CHECK(LCompilers::pickle(*asr) == "(TranslationUnit (SymbolTable 1 {f: (Function (SymbolTable 2 {f: (Variable 2 f [] ReturnVar () () Default (Integer 4) () Source Public Required .false. .false. .false. () .false. .false. .false. NotMethod () [] ())}) f (FunctionType [] (Integer 4) Source Implementation () .false. .false. .false. .false. .false. [] .false. Host) [] [] [(Assignment (Var 2 f) (IntegerConstant 5 (Integer 4) Decimal) () .false. .false. .false. .false.)] (Var 2 f) Public .true. .true. ())}) [] ())");

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
    CHECK(LCompilers::pickle(*asr) == "(TranslationUnit (SymbolTable 3 {f: (Function (SymbolTable 4 {f: (Variable 4 f [] ReturnVar () () Default (Integer 4) () Source Public Required .false. .false. .false. () .false. .false. .false. NotMethod () [] ())}) f (FunctionType [] (Integer 4) Source Implementation () .false. .false. .false. .false. .false. [] .false. Host) [] [] [(Assignment (Var 4 f) (IntegerConstant 4 (Integer 4) Decimal) () .false. .false. .false. .false.)] (Var 4 f) Public .true. .true. ())}) [] ())");
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

TEST_CASE("ASR descriptor entry capability preserves direct specialization") {
    std::string source = R"(module entry_capability_m
implicit none
contains
subroutine consume(a, r)
    real, intent(in) :: a(:)
    real, intent(out) :: r
    r = a(1) + a(size(a))
end subroutine
end module
program caller
use entry_capability_m
implicit none
real :: a(2), r
call consume(a, r)
end program)";
    namespace ASR = LCompilers::ASR;
    namespace ASRUtils = LCompilers::ASRUtils;
    for (bool supported: {false, true}) {
        CAPTURE(supported);
        Allocator al(256*1024);
        LCompilers::diag::Diagnostics diagnostics;
        CompilerOptions co;
        co.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
        co.po.supports_descriptor_entry_wrappers = supported;
        co.po.skip_removal_of_unused_procedures_in_pass_array_by_data = true;
        co.po.verify_all_passes = true;
        LCompilers::LocationManager lm;
        LCompilers::LocationManager::FileLocations file;
        file.in_filename = "entry_capability.f90";
        lm.files.push_back(file);
        lm.file_ends.push_back(LCompilers::LFortran::source_offset(source.size()));
        auto *ast = TRY(LCompilers::LFortran::parse(al, source, diagnostics, co));
        auto *asr = TRY(LCompilers::LFortran::ast_to_asr(al, *ast, diagnostics,
            nullptr, false, co, lm));
        auto *module = ASR::down_cast<ASR::Module_t>(
            asr->m_symtab->get_symbol("entry_capability_m"));
        auto *entry = ASR::down_cast<ASR::Function_t>(
            module->m_symtab->get_symbol("consume"));
        REQUIRE(entry->n_body == 1);
        auto *original_body = entry->m_body[0];
        LCompilers::PassManager passes;
        std::string pass = "pass_array_by_data", skip;
        passes.parse_pass_arg(pass, skip);
        co.po.pass_cumulative = true;
        passes.apply_passes(al, asr, co.po, diagnostics);
        INFO(diagnostics.render2());
        REQUIRE_FALSE(diagnostics.has_error());
        REQUIRE(LCompilers::asr_verify(*asr, true, diagnostics));
        CHECK(module->m_symtab->get_symbol("consume") == &entry->base);
        CHECK(entry->n_args == 2);
        auto *caller = ASR::down_cast<ASR::Program_t>(
            asr->m_symtab->get_symbol("caller"));
        REQUIRE(caller->n_body == 1);
        auto *call = ASR::down_cast<ASR::SubroutineCall_t>(caller->m_body[0]);
        auto *clone = ASR::down_cast<ASR::Function_t>(
            ASRUtils::symbol_get_past_external(call->m_name));
        REQUIRE(clone != entry);
        CHECK(clone->n_args > entry->n_args);
        REQUIRE(entry->n_body == 1);
        if (supported) {
            auto *delegation = ASR::down_cast<ASR::SubroutineCall_t>(
                entry->m_body[0]);
            CHECK(ASRUtils::symbol_get_past_external(delegation->m_name)
                == &clone->base);
        } else {
            CHECK(entry->m_body[0] == original_body);
            CHECK(ASR::is_a<ASR::Assignment_t>(*entry->m_body[0]));
        }
    }
}

TEST_CASE("ASR -> LLVM lowers DoLoop with loop-else and EXIT") {
    std::string source = R"(
integer function f_normal()
    integer :: i
    f_normal = 0
    do i = 1, 3
        f_normal = f_normal + 1
    end do
    f_normal = f_normal + 10
    f_normal = f_normal + 100 * i
end function

integer function f_zero_trip()
    integer :: i
    f_zero_trip = 0
    do i = 1, 0
        f_zero_trip = f_zero_trip + 1
    end do
    f_zero_trip = f_zero_trip + 10
    f_zero_trip = f_zero_trip + 100 * i
end function

integer function f_exit()
    integer :: i
    f_exit = 0
    do i = 1, 3
        f_exit = f_exit + 1
        exit
    end do
    f_exit = f_exit + 10
    f_exit = f_exit + 100 * i
end function

integer function f_while_nested_exit()
    integer :: i, s
    s = 0
    do while (s < 2)
        do i = 1, 3
            exit
        end do
        s = s + 1
    end do
    s = s + 40
    f_while_nested_exit = s
end function
)";

    Allocator al(64*1024);
    LCompilers::diag::Diagnostics diagnostics;
    CompilerOptions co;
    co.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    co.platform = LCompilers::get_platform();
    FortranEvaluator evaluator(co);
    LCompilers::LocationManager lm;
    LCompilers::LocationManager::FileLocations file;
    file.in_filename = "doloop_else.f90";
    lm.files.push_back(file);
    lm.file_ends.push_back(LCompilers::LFortran::source_offset(source.size()));
    auto parsed = evaluator.get_asr2(source, lm, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE(parsed.ok);
    LCompilers::ASR::TranslationUnit_t *asr = parsed.result;

    // Fortran has no loop-else, so move the statement that follows the
    // first loop of `name` into the loop-else of that loop: it then runs
    // only when the loop completes normally (not after an EXIT).
    auto seed_orelse_from_next_stmt = [&](const std::string &name,
                                          bool while_loop) {
        LCompilers::ASR::symbol_t *sym = asr->m_symtab->resolve_symbol(name);
        REQUIRE(sym != nullptr);
        REQUIRE(LCompilers::ASR::is_a<LCompilers::ASR::Function_t>(*sym));
        auto *fn = LCompilers::ASR::down_cast<LCompilers::ASR::Function_t>(sym);
        size_t loop_idx = fn->n_body;
        for (size_t i = 0; i < fn->n_body; i++) {
            bool matched = while_loop
                ? LCompilers::ASR::is_a<LCompilers::ASR::WhileLoop_t>(*fn->m_body[i])
                : LCompilers::ASR::is_a<LCompilers::ASR::DoLoop_t>(*fn->m_body[i]);
            if (matched) {
                loop_idx = i;
                break;
            }
        }
        REQUIRE(loop_idx < fn->n_body);
        REQUIRE(loop_idx + 1 < fn->n_body);
        LCompilers::ASR::stmt_t *orelse_stmt = fn->m_body[loop_idx + 1];
        auto **orelse_block = al.allocate<LCompilers::ASR::stmt_t*>(1);
        orelse_block[0] = orelse_stmt;
        if (while_loop) {
            auto *loop = LCompilers::ASR::down_cast<LCompilers::ASR::WhileLoop_t>(
                fn->m_body[loop_idx]);
            LCompilers::ASR::WhileLoop_t &loop_mut = *loop;
            loop_mut.m_orelse = orelse_block;
            loop_mut.n_orelse = 1;
        } else {
            auto *loop = LCompilers::ASR::down_cast<LCompilers::ASR::DoLoop_t>(
                fn->m_body[loop_idx]);
            LCompilers::ASR::DoLoop_t &loop_mut = *loop;
            loop_mut.m_orelse = orelse_block;
            loop_mut.n_orelse = 1;
        }
        for (size_t i = loop_idx + 1; i + 1 < fn->n_body; i++) {
            fn->m_body[i] = fn->m_body[i + 1];
        }
        fn->n_body--;
    };

    seed_orelse_from_next_stmt("f_normal", false);
    seed_orelse_from_next_stmt("f_zero_trip", false);
    seed_orelse_from_next_stmt("f_exit", false);
    seed_orelse_from_next_stmt("f_while_nested_exit", true);

    auto function_has_doloop = [&](const std::string &name) {
        LCompilers::ASR::symbol_t *sym = asr->m_symtab->resolve_symbol(name);
        REQUIRE(sym != nullptr);
        auto *fn = LCompilers::ASR::down_cast<LCompilers::ASR::Function_t>(sym);
        for (size_t i = 0; i < fn->n_body; i++) {
            if (LCompilers::ASR::is_a<LCompilers::ASR::DoLoop_t>(*fn->m_body[i])) {
                return true;
            }
        }
        return false;
    };
    CHECK(function_has_doloop("f_normal"));
    CHECK(function_has_doloop("f_zero_trip"));
    CHECK(function_has_doloop("f_exit"));

    LCompilers::LLVMEvaluator e(co);
    LCompilers::PassManager lpm;
    lpm.use_default_passes();
    LCompilers::Result<std::unique_ptr<LCompilers::LLVMModule>> res =
        LCompilers::asr_to_llvm(*asr, diagnostics, e.get_context(),
            e.get_target_config(), al, lpm, co, "", "", "", lm);
    INFO(diagnostics.render2());
    REQUIRE(res.ok);

    // The passes kept the loops for the backend.
    CHECK(function_has_doloop("f_normal"));
    CHECK(function_has_doloop("f_zero_trip"));
    CHECK(function_has_doloop("f_exit"));

    e.add_module(std::move(res.result));
    CHECK(e.execfn<int32_t>("f_normal") == 413);
    CHECK(e.execfn<int32_t>("f_zero_trip") == 110);
    CHECK(e.execfn<int32_t>("f_exit") == 101);
    CHECK(e.execfn<int32_t>("f_while_nested_exit") == 42);
}

namespace {

// Compiles `source` with the LLVM backend, with --fast if `fast`, and calls
// `inspect` with each of the given procedures and its LLVM definition, before
// any LLVM pass runs, unless `optimize` is set. Each procedure is given as
// {name, end of the name of its LLVM definition}.
void inspect_definitions(const std::string &source, bool fast,
        const std::vector<std::pair<std::string, std::string>> &procedures,
        const std::function<void(const std::string &, llvm::Function &)>
            &inspect, bool stack_arrays = false, bool optimize = false,
            bool detect_leaks = false,
        const std::function<void(CompilerOptions &)> &configure = {}) {
    Allocator al(64*1024);
    LCompilers::diag::Diagnostics diagnostics;
    CompilerOptions co;
    co.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
    co.platform = LCompilers::get_platform();
    co.po.fast = fast;
    co.po.stack_arrays = stack_arrays;
    if (fast || optimize) {
        co.po.bounds_checking = false;
        co.po.strict_bounds_checking = false;
    }
    co.detect_leaks = detect_leaks;
    if (configure) configure(co);
    LCompilers::LFortran::AST::TranslationUnit_t* tu = TRY(
        LCompilers::LFortran::parse(al, source, diagnostics, co));
    LCompilers::LocationManager lm;
    LCompilers::LocationManager::FileLocations file;
    file.in_filename = "inspect.f90";
    lm.files.push_back(file);
    lm.file_ends.push_back(LCompilers::LFortran::source_offset(source.size()));
    LCompilers::ASR::TranslationUnit_t* asr = TRY(
        LCompilers::LFortran::ast_to_asr(al, *tu, diagnostics, nullptr, false,
            co, lm));
    LCompilers::LLVMEvaluator e;
    LCompilers::PassManager lpm;
    lpm.use_default_passes();
    LCompilers::Result<std::unique_ptr<LCompilers::LLVMModule>>
        res = LCompilers::asr_to_llvm(*asr, diagnostics, e.get_context(),
            e.get_target_config(), al, lpm, co, "", "", "", lm);
    REQUIRE(res.ok);
    if (optimize) {
        e.opt(*res.result->m_m);
    }
    for (const auto &[procedure, suffix]: procedures) {
        llvm::Function *fn = nullptr;
        for (llvm::Function &f: *res.result->m_m) {
            std::string name = f.getName().str();
            if (!f.isDeclaration() && name.size() >= suffix.size() &&
                    name.compare(name.size() - suffix.size(),
                        suffix.size(), suffix) == 0) {
                fn = &f;
            }
        }
        REQUIRE(fn != nullptr);
        inspect(procedure, *fn);
    }
}

TEST_CASE("LLVM statement-local stack storage for numeric function results") {
    std::string source = R"(module stack_results
implicit none
type :: sigmoid
contains
    procedure :: evaluate, derivative, consume
end type
contains
pure function evaluate(self, x) result(y)
    class(sigmoid), intent(in) :: self
    real, intent(in) :: x(:)
    real :: y(size(x))
    y = 1.0 / (1.0 + exp(-x))
end function
pure function derivative(self, x) result(y)
    class(sigmoid), intent(in) :: self
    real, intent(in) :: x(:)
    real :: y(size(x))
    y = self%evaluate(x) * (1.0 - self%evaluate(x))
end function
subroutine consume(self, y, x)
    class(sigmoid), intent(in) :: self
    real, intent(inout) :: y(:)
    real, intent(in) :: x(:)
    y = y - x
end subroutine
subroutine argument_expression(self, y, x, scale)
    class(sigmoid), intent(in) :: self
    real, intent(inout) :: y(:)
    real, intent(in) :: x(:), scale
    call self%consume(y, x / scale)
end subroutine
subroutine pointer_argument_expression(self, y, x, scale)
    class(sigmoid), intent(in) :: self
    real, intent(inout) :: y(:)
    real, pointer, intent(in) :: x(:)
    real, intent(in) :: scale
    call self%consume(y, x / scale)
end subroutine
subroutine user_allocation(n, y)
    integer, intent(in) :: n
    real, allocatable, intent(out) :: y(:)
    allocate(y(n))
    y = 1.0
end subroutine
end module
)";
    for (bool fast: {false, true}) {
        unsigned heap_calls[3][2] = {};
        for (bool stack: {false, true}) {
            inspect_definitions(source, fast,
                {{"derivative", "_derivative"},
                 {"argument_expression", "_argument_expression_real____1_real____2"},
                 {"pointer_argument_expression", "_pointer_argument_expression"},
                 {"user_allocation", "_user_allocation"}},
                [&](const std::string &name, llvm::Function &fn) {
                    unsigned allocations = 0, saves = 0, restores = 0;
                    unsigned dynamic_arrays = 0, array_calls = 0;
                    for (llvm::BasicBlock &block: fn) {
                        for (llvm::Instruction &inst: block) {
                            if (auto *allocation = llvm::dyn_cast<llvm::AllocaInst>(&inst)) {
                                if (allocation->getAllocatedType()->isFloatTy()
                                        && !llvm::isa<llvm::ConstantInt>(
                                            allocation->getArraySize())) {
                                    dynamic_arrays++;
                                }
                            }
                            auto *call = llvm::dyn_cast<llvm::CallInst>(&inst);
                            if (!call) continue;
                            if (!call->getCalledFunction()) {
                                array_calls++;
                                continue;
                            }
                            llvm::StringRef callee = call->getCalledFunction()->getName();
                            if (callee == "_lfortran_malloc_alloc") allocations++;
                            auto intrinsic = call->getCalledFunction()->getIntrinsicID();
                            if (intrinsic == llvm::Intrinsic::stacksave) saves++;
                            if (intrinsic == llvm::Intrinsic::stackrestore) restores++;
                        }
                    }
                    if (name == "user_allocation") {
                        CHECK(allocations > 0);
                    } else {
                        unsigned index = name == "derivative" ? 0 :
                            name == "argument_expression" ? 1 : 2;
                        heap_calls[index][stack] = allocations;
                        if (stack) {
                            if (name != "derivative") {
                                CHECK(array_calls == 1);
                                CHECK(allocations == 0);
                            }
                            CHECK(array_calls > 0);
                            CHECK(dynamic_arrays == array_calls);
                            CHECK(saves == array_calls);
                            CHECK(restores == saves);
                        }
                    }
                }, stack);
        }
        for (auto& calls: heap_calls) {
            CHECK(calls[1] < calls[0]);
        }
    }
}

TEST_CASE("LLVM reuses repeated pure array calls within arithmetic assignments") {
    std::string source = R"(module pure_array_calls
implicit none
type :: activation
contains
    procedure :: evaluate
end type
contains
pure function evaluate(self, x) result(y)
    class(activation), intent(in) :: self
    real, intent(in) :: x(:)
    real :: y(size(x))
    y = x + 1.0
end function
subroutine repeated(self, x, y)
    class(activation), intent(in) :: self
    real, intent(in) :: x(:)
    real, intent(out) :: y(:)
    y = self%evaluate(x) * (1.0 - self%evaluate(x))
end subroutine
subroutine different_arguments(self, x, z, y)
    class(activation), intent(in) :: self
    real, intent(in) :: x(:), z(:)
    real, intent(out) :: y(:)
    y = self%evaluate(x) + self%evaluate(z)
end subroutine
subroutine different_receivers(self, other, x, y)
    class(activation), intent(in) :: self, other
    real, intent(in) :: x(:)
    real, intent(out) :: y(:)
    y = self%evaluate(x) + other%evaluate(x)
end subroutine
function impure_scalar() result(x)
    real :: x
    x = 1.0
end function
subroutine impure_surroundings(self, x, y)
    class(activation), intent(in) :: self
    real, intent(in) :: x(:)
    real, intent(out) :: y(:)
    y = self%evaluate(x) * (1.0 - self%evaluate(x)) + impure_scalar()
end subroutine
subroutine volatile_actual(self, x, y)
    class(activation), intent(in) :: self
    real, volatile, intent(inout) :: x(:)
    real, intent(out) :: y(:)
    y = self%evaluate(x) * (1.0 - self%evaluate(x))
end subroutine
subroutine asynchronous_actual(self, x, y)
    class(activation), intent(in) :: self
    real, asynchronous, intent(in) :: x(:)
    real, intent(out) :: y(:)
    y = self%evaluate(x) * (1.0 - self%evaluate(x))
end subroutine
subroutine target_actual(self, x, y)
    class(activation), intent(in) :: self
    real, target, intent(in) :: x(:)
    real, intent(out) :: y(:)
    y = self%evaluate(x) * (1.0 - self%evaluate(x))
end subroutine
subroutine pointer_actual(self, x, y)
    class(activation), intent(in) :: self
    real, pointer, intent(in) :: x(:)
    real, intent(out) :: y(:)
    y = self%evaluate(x) * (1.0 - self%evaluate(x))
end subroutine
pure function invoke(callback, x) result(y)
    interface
        pure real function callback(x)
            real, intent(in) :: x
        end function
    end interface
    real, intent(in) :: x(:)
    real :: y(size(x))
    y = x + callback(1.0)
end function
subroutine procedure_actual(callback, x, y)
    interface
        pure real function callback(x)
            real, intent(in) :: x
        end function
    end interface
    real, intent(in) :: x(:)
    real, intent(out) :: y(:)
    y = invoke(callback, x) + invoke(callback, x)
end subroutine
end module
)";
    for (int mode = 0; mode < 7; mode++) {
        // With a GPU device the body of a module subroutine without a
        // target, pointer or procedure dummy is in its deferring twin
        // (gpu_launch_sync.cpp).
        std::string twin = mode == 4 || mode == 5 ? "__gpu_deferred" : "";
        inspect_definitions(source, mode != 0,
            {{"repeated", "_repeated_real____1_real____2" + twin},
             {"different_arguments", "_different_arguments_real____1_real____2_real____3" + twin},
             {"different_receivers", "_different_receivers_real____2_real____3" + twin},
             {"impure_surroundings", "_impure_surroundings_real____1_real____2" + twin},
             {"volatile_actual", "_volatile_actual_real____1_real____2" + twin},
             {"asynchronous_actual", "_asynchronous_actual_real____1_real____2" + twin},
             {"target_actual", "_target_actual_real____2"},
             {"pointer_actual", "_pointer_actual"},
             {"procedure_actual", "_procedure_actual_real____1_real____2"}},
            [&](const std::string &name, llvm::Function &fn) {
                unsigned indirect_calls = 0;
                for (llvm::BasicBlock &block: fn) {
                    for (llvm::Instruction &inst: block) {
                        auto *call = llvm::dyn_cast<llvm::CallInst>(&inst);
                        if (call && (!call->getCalledFunction() ||
                                (name == "procedure_actual" &&
                                call->getCalledFunction()->getName().contains("invoke")))) {
                            indirect_calls++;
                        }
                    }
                }
                INFO("mode: " << mode << ", procedure: " << name);
                CHECK(indirect_calls == ((mode == 1 || mode == 6) &&
                    name == "repeated" ? 1 : 2));
            }, false, false, false,
            [&](CompilerOptions &co) {
                co.po.openmp = mode == 2;
                co.po.coarray = mode == 3;
                co.po.gpu_offload_metal = mode == 4;
                co.po.gpu_offload_cuda = mode == 5;
                co.po.stack_arrays = mode == 6;
                co.po.verify_all_passes = true;
            });
    }
}

TEST_CASE("LLVM internal procedures have internal linkage") {
    // With --fast, `integrate` is copied into `host` as the internal procedure
    // `integrate__f`, which calls `f` directly. Only `host` can name either,
    // so neither is exported, and once both are inlined into `host`, LLVM
    // deletes them.
    std::string source = R"(module integ_m
implicit none
abstract interface
    real(8) function fun(x)
        real(8), intent(in) :: x
    end function
end interface
contains
    real(8) function integrate(f, a, b, n) result(s)
        procedure(fun) :: f
        real(8), intent(in) :: a, b
        integer, intent(in) :: n
        integer :: i
        real(8) :: h
        h = (b - a)/n
        s = 0
        do i = 1, n
            s = s + f(a + (i - 0.5d0)*h)
        end do
        s = s*h
    end function
end module

module host_m
use integ_m, only: integrate
implicit none
contains
    subroutine host(p, q, r, n, res)
        real(8), intent(in) :: p, q, r
        integer, intent(in) :: n
        real(8), intent(out) :: res
        res = integrate(f, 0d0, 1d0, n)
    contains
        real(8) function f(x)
            real(8), intent(in) :: x
            f = (p*x + q)*x + r
        end function
    end subroutine
end module
)";
    for (bool fast: {false, true}) {
        CAPTURE(fast);
        std::vector<std::pair<std::string, std::string>> procedures = {
            {"host", "_m_host"}, {"f", "_m_f"}};
        if (fast) procedures.push_back({"integrate__f", "_integrate__f"});
        inspect_definitions(source, fast, procedures,
            [&](const std::string &procedure, llvm::Function &fn) {
                CAPTURE(procedure);
                CHECK(fn.hasInternalLinkage() == (procedure != "host"));
            });
    }
    inspect_definitions(source, true, {{"host", "_m_host"}},
        [&](const std::string &, llvm::Function &fn) {
            for (llvm::Function &other: *fn.getParent()) {
                std::string name = other.getName().str();
                CAPTURE(name);
                CHECK(name.rfind("host.", 0) != 0);
            }
        }, false, true);
}

TEST_CASE("LLVM rank-one SIZE avoids a runtime rank loop") {
    std::string source = R"(module size_calls
implicit none
contains
function extent(x) result(n)
    real, target, intent(in) :: x(:)
    integer(8) :: n
    n = size(x, kind=8)
end function
function matrix_extent(x) result(n)
    real, target, intent(in) :: x(:, :)
    integer(8) :: n
    n = size(x, kind=8)
end function
function dynamic_extent(x) result(n)
    real, target, intent(in) :: x(..)
    integer(8) :: n
    n = size(x, kind=8)
end function
end module
)";
    for (bool fast: {false, true}) {
        inspect_definitions(source, fast,
            {{"extent", "_calls_extent"},
             {"matrix_extent", "_matrix_extent"},
             {"dynamic_extent", "_dynamic_extent"}},
            [&](const std::string &name, llvm::Function &fn) {
                unsigned conditional_branches = 0;
                for (llvm::BasicBlock &block: fn) {
#if LLVM_VERSION_MAJOR >= 23
                    if (llvm::isa<llvm::CondBrInst>(block.getTerminator())) {
                        conditional_branches++;
                    }
#else
                    if (auto* branch = llvm::dyn_cast<llvm::BranchInst>(
                            block.getTerminator())) {
                        if (branch->isConditional()) conditional_branches++;
                    }
#endif
                }
                INFO(name, ", fast=", fast);
                CHECK(conditional_branches == (fast && name == "extent" ? 0 : 1));
            });
    }
}

TEST_CASE("LLVM stack automatic arrays allocate their element type") {
    std::string source = R"(module stack_alloca_m
implicit none
contains
subroutine work(n, x)
    integer, intent(in) :: n
    real, intent(out) :: x
    real :: w(n)
    integer(8) :: k(n, 2)
    w = 1
    k = 2
    x = sum(w) + real(sum(k))
end subroutine
end module
)";
    for (bool fast: {false, true}) {
        inspect_definitions(source, fast, {{"work", "work"}},
            [&](const std::string &, llvm::Function &fn) {
                std::map<std::string, std::string> types;
                for (llvm::Instruction &inst: fn.getEntryBlock()) {
                    auto *alloca = llvm::dyn_cast<llvm::AllocaInst>(&inst);
                    if (!alloca) continue;
                    std::string name = alloca->getName().str();
                    if (name != "w" && name != "k") continue;
                    std::string type;
                    llvm::raw_string_ostream os(type);
                    alloca->getAllocatedType()->print(os);
                    types[name] = os.str();
                }
                CAPTURE(fast);
                CHECK(types["w"] == "float");
                CHECK(types["k"] == "i64");
            }, true);
    }
}

// The dummy arguments, as "procedure:argument ...", whose parameters the LLVM
// backend marks noalias when it compiles `source`.
std::string noalias_parameters(const std::string &source, bool fast,
        const std::vector<std::pair<std::string, std::string>> &procedures) {
    std::string result;
    inspect_definitions(source, fast, procedures,
        [&](const std::string &procedure, llvm::Function &fn) {
            for (llvm::Argument &arg: fn.args()) {
                if (arg.hasNoAliasAttr()) {
                    result += procedure + ":" + arg.getName().str() + " ";
                }
            }
        });
    return result;
}

// The kinds of loads and stores with TBAA metadata in the LLVM definitions
// of `procedures`, as sorted "procedure:load|store pointer|<type>:<TBAA
// type>; ...".
std::string tbaa_accesses(const std::string &source, bool fast,
        const std::vector<std::pair<std::string, std::string>> &procedures) {
    std::set<std::string> result;
    inspect_definitions(source, fast, procedures,
        [&](const std::string &procedure, llvm::Function &fn) {
            for (llvm::BasicBlock &bb: fn) {
                for (llvm::Instruction &inst: bb) {
                    llvm::MDNode *tag = inst.getMetadata(
                        llvm::LLVMContext::MD_tbaa);
                    if (tag == nullptr) {
                        continue;
                    }
                    llvm::Type *type = nullptr;
                    std::string access;
                    if (llvm::LoadInst *load =
                            llvm::dyn_cast<llvm::LoadInst>(&inst)) {
                        type = load->getType();
                        access = "load";
                    } else if (llvm::StoreInst *store =
                            llvm::dyn_cast<llvm::StoreInst>(&inst)) {
                        type = store->getValueOperand()->getType();
                        access = "store";
                    }
                    REQUIRE(type != nullptr);
                    std::string type_name = "pointer";
                    if (!type->isPointerTy()) {
                        llvm::raw_string_ostream os(type_name);
                        type_name.clear();
                        type->print(os);
                        os.flush();
                    }
                    llvm::MDNode *access_type = llvm::cast<llvm::MDNode>(
                        tag->getOperand(1));
                    llvm::MDNode *root = access_type;
                    while (root->getNumOperands() > 1) {
                        root = llvm::cast<llvm::MDNode>(root->getOperand(1));
                    }
                    REQUIRE(llvm::cast<llvm::MDString>(root->getOperand(0))
                        ->getString() == "LFortran TBAA");
                    result.insert(procedure + ":" + access + " " + type_name
                        + ":" + llvm::cast<llvm::MDString>(
                            access_type->getOperand(0))->getString().str());
                }
            }
        });
    std::string joined;
    for (const std::string &access: result) {
        joined += access + "; ";
    }
    return joined;
}

}

TEST_CASE("LLVM noalias dummy arguments under --fast") {
    std::string source = R"(module noalias_ir_m
use iso_c_binding, only: c_ptr, c_f_pointer
implicit none
type :: t
    real, allocatable :: c(:)
end type
contains

subroutine plain(y, x, n, s)
    integer, intent(in) :: n
    real, intent(inout) :: y(n)
    real, intent(in) :: x(n)
    real, intent(out) :: s
    y(1) = x(1) + n
    s = y(1)
end subroutine

subroutine assumed(x, y, str)
    real, intent(in) :: x(:)
    real, intent(inout) :: y(:)
    character(len=*), intent(in) :: str
    y(1) = x(1) + len(str)
end subroutine

subroutine attributes(p, tg, v, as, k, r)
    real, pointer, intent(in) :: p
    real, target, intent(in) :: tg
    real, volatile :: v
    real, asynchronous :: as
    integer, value :: k
    real, intent(out) :: r
    r = p + tg + v + as + k
end subroutine

subroutine last_inout(x, y)
    real, intent(in) :: x(3)
    real, intent(inout) :: y(3)
    y(2) = x(2)
end subroutine

subroutine cptr(p, q, r)
    type(c_ptr), intent(in) :: p, q
    real, intent(out) :: r
    real, pointer :: a, b
    call c_f_pointer(p, a)
    call c_f_pointer(q, b)
    a = 1
    b = 2
    r = a
end subroutine

subroutine scoped_async(a)
    real :: a(4)
    block
        asynchronous :: a
        a = 1
    end block
end subroutine

subroutine scoped_volatile(a)
    real :: a(4)
    block
        volatile :: a
        a = 1
    end block
end subroutine

subroutine scoped_contained(a)
    real :: a(4)
    call inner()
contains
    subroutine inner()
        asynchronous :: a
        a = 1
    end subroutine
end subroutine

subroutine shadow(a)
    real :: a(4)
    block
        asynchronous :: a
        real :: a(4)
        a = 1
    end block
    a = 2
end subroutine

function f(x) result(res)
    real, intent(in) :: x(3)
    real :: res(3)
    res = x + 1
end function

subroutine self_first(self, i, v)
    class(t), intent(inout) :: self
    integer, intent(in) :: i
    real, intent(out) :: v
    v = self%c(i)
end subroutine

subroutine bits(i, j)
    integer, intent(inout) :: i, j
    call mvbits(i, 0, 4, j, 4)
end subroutine

subroutine bind_c(x) bind(c)
    real, intent(inout) :: x
    x = 1
end subroutine

end module
)";
    std::vector<std::pair<std::string, std::string>> procedures = {
        {"plain", "_m_plain"},
        {"assumed", "_m_assumed_real____0_real____1"},
        {"attributes", "_m_attributes"},
        {"last_inout", "_m_last_inout"},
        {"cptr", "_m_cptr"},
        {"scoped_async", "_m_scoped_async"},
        {"scoped_volatile", "_m_scoped_volatile"},
        {"scoped_contained", "_m_scoped_contained"},
        {"shadow", "_m_shadow"},
        {"f", "_m_f"},
        {"self_first", "_m_self_first"},
        {"bits", "_m_bits"},
        {"mvbits", "_lcompilers_mvbits_integer"},
        {"bind_c", "bind_c"}};
    std::string expected =
        "plain:y plain:x plain:n plain:s "
        // pass_array_by_data passes the data and the extents of x and y.
        "assumed:x assumed:__1x assumed:y assumed:__1y "
        "assumed:str "
        // Not POINTER p, TARGET tg, VOLATILE v, ASYNCHRONOUS as or VALUE k.
        "attributes:r "
        "last_inout:x last_inout:y "
        // C addresses may designate the same storage. Scoped attributes
        // exclude a, but a locally redeclared name does not affect its host.
        "cptr:r shadow:a "
        // Not res, the function result.
        "f:x "
        "self_first:self self_first:i self_first:v "
        "bits:i bits:j "
        // Not FROM and TO of MVBITS, which can be the same variable; nor
        // the dummy argument of the bind(c) procedure.
        "mvbits:frompos mvbits:len mvbits:topos ";
    CHECK(noalias_parameters(source, true, procedures) == expected);
    CHECK(noalias_parameters(source, false, procedures).empty());
}

TEST_CASE("LLVM signed integer arithmetic is nsw under --fast") {
    std::string source = R"(module integer_nsw_m
implicit none
contains
integer function combine(a, b, c) result(r)
    integer, intent(in) :: a, b, c
    r = a*b + c - a
end function
end module
)";
    for (bool fast: {false, true}) {
        inspect_definitions(source, fast, {{"combine", "_combine"}},
            [&](const std::string &, llvm::Function &fn) {
                size_t ops = 0, nsw = 0;
                for (llvm::BasicBlock &block: fn) {
                    for (llvm::Instruction &inst: block) {
                        auto *op = llvm::dyn_cast<llvm::BinaryOperator>(&inst);
                        if (!op || !op->getType()->isIntegerTy(32)) continue;
                        unsigned opcode = op->getOpcode();
                        if (opcode != llvm::Instruction::Add &&
                                opcode != llvm::Instruction::Sub &&
                                opcode != llvm::Instruction::Mul) continue;
                        ops++;
                        if (op->hasNoSignedWrap()) nsw++;
                    }
                }
                CAPTURE(fast);
                CHECK(ops == 3);
                CHECK(nsw == (fast ? 3u : 0u));
            });
    }
}

TEST_CASE("LLVM loops are not runtime unrolled under --fast") {
    std::string source = R"(module runtime_unroll_m
implicit none
contains
subroutine window(x, y, i0, i1)
    real, intent(in) :: x(:,:)
    real, intent(inout) :: y(:,:)
    integer, intent(in) :: i0, i1
    integer :: j
    do j = 1, size(y, 2)
        y(:, j) = y(:, j) + x(i0:i1, j)
    end do
end subroutine
end module
)";
    for (bool fast: {false, true}) {
        inspect_definitions(source, fast,
            {{"window", "_window_real_______0_real_______1"}},
            [&](const std::string &, llvm::Function &fn) {
                size_t latches = 0, rolled = 0;
                for (llvm::BasicBlock &block: fn) {
                    llvm::MDNode *loop = block.getTerminator()->getMetadata(
                        llvm::LLVMContext::MD_loop);
                    if (!loop) continue;
                    latches++;
                    for (const llvm::MDOperand &op: loop->operands()) {
                        auto *node = llvm::dyn_cast_or_null<llvm::MDNode>(op.get());
                        if (!node || node->getNumOperands() == 0) continue;
                        auto *name = llvm::dyn_cast<llvm::MDString>(node->getOperand(0));
                        if (name && name->getString() ==
                                "llvm.loop.unroll.runtime.disable") {
                            rolled++;
                        }
                    }
                }
                CAPTURE(fast);
                CHECK(rolled == latches);
                // Both the DO loop and the array-operation loop it contains.
                CHECK(latches == (fast ? 2u : 0u));
            });
    }
}

TEST_CASE("LLVM straight-line code is SLP vectorized under --fast") {
    // Four independent additions of adjacent elements: only the SLP
    // vectorizer can combine them, as there is no loop to vectorize.
    std::string source = R"(module slp_m
implicit none
contains
subroutine add4(a, b, c)
    real(8), intent(in) :: a(4), b(4)
    real(8), intent(out) :: c(4)
    c(1) = a(1) + b(1)
    c(2) = a(2) + b(2)
    c(3) = a(3) + b(3)
    c(4) = a(4) + b(4)
end subroutine
end module
)";
    inspect_definitions(source, true, {{"add4", "_slp_m_add4"}},
        [&](const std::string &, llvm::Function &fn) {
            size_t scalar_adds = 0, vector_adds = 0;
            for (llvm::BasicBlock &block: fn) {
                for (llvm::Instruction &inst: block) {
                    if (inst.getOpcode() != llvm::Instruction::FAdd) continue;
                    if (inst.getType()->isVectorTy()) {
                        vector_adds++;
                    } else {
                        scalar_adds++;
                    }
                }
            }
            CHECK(vector_adds > 0);
            CHECK(scalar_adds == 0);
        }, false, true);
}

TEST_CASE("LLVM TBAA metadata under --fast") {
    std::string source = R"(module tbaa_ir_m
implicit none
type :: layer_t
    real, allocatable :: w(:,:)
    real, allocatable :: out(:)
end type
contains

subroutine forward(self, x)
    class(layer_t), intent(inout) :: self
    real, intent(in) :: x(:)
    integer :: i, j
    do j = 1, size(self%w, 2)
        do i = 1, size(self%w, 1)
            self%out(i) = self%out(i) + self%w(i, j)*x(j)
        end do
    end do
end subroutine

subroutine pun(x, k)
    real, intent(in) :: x(4)
    integer, intent(out) :: k(4)
    k = transfer(x, k)
end subroutine

subroutine bind_c(x, n) bind(c)
    integer, value :: n
    real, intent(out) :: x
    real, allocatable :: a(:)
    allocate(a(n))
    a(n) = 1
    x = a(n)
end subroutine

end module
)";
    std::vector<std::pair<std::string, std::string>> procedures = {
        // pass_array_by_data passes the data and the extent of x.
        {"forward", "_m_forward_real____1"},
        {"pun", "_m_pun"},
        {"bind_c", "bind_c"}};
    // Nothing in the bind(c) procedure.
    std::string expected =
        // Elements of self%out and self%w.
        "forward:load float:allocated component; "
        // Elements of x, which is passed by its data pointer.
        "forward:load float:data; "
        // Offsets, lower bounds and strides of the descriptors.
        "forward:load i64:descriptor; "
        // The class wrapper (self), the descriptor pointers of the
        // components and the base addresses of their data.
        "forward:load pointer:descriptor; "
        "forward:store float:allocated component; "
        // TRANSFER reinterprets each element through an untagged scalar;
        // only the elements of the explicit-shape dummy argument x and the
        // integer elements of the local fixed-size temporary that holds its
        // result are tagged.
        "pun:load float:data; "
        "pun:load i32:data; "
        "pun:store i32:data; ";
    CHECK(tbaa_accesses(source, true, procedures) == expected);
    CHECK(tbaa_accesses(source, false, procedures).empty());
}

TEST_CASE("LLVM contiguous array-operation indexing under --fast") {
    std::string source = R"(module contiguous_index_m
implicit none
type :: storage_t
    real, allocatable :: a(:,:)
    real, pointer :: p(:,:)
end type
contains
subroutine scale_allocatable(self)
    class(storage_t), intent(inout) :: self
    self%a = 2*self%a
end subroutine
subroutine scale_pointer(self)
    class(storage_t), intent(inout) :: self
    self%p = 2*self%p
end subroutine
subroutine scale_allocatable_section(self)
    class(storage_t), intent(inout), target :: self
    integer :: j
    do j = 1, size(self%a,2)
        self%a(:,j) = 2*self%a(:,j)
    end do
end subroutine
end module
)";
    for (bool fast: {false, true}) {
        inspect_definitions(source, fast,
            {{"allocatable", "_m_scale_allocatable"},
             {"pointer", "_m_scale_pointer"},
             {"allocatable_section", "_m_scale_allocatable_section"}},
            [&](const std::string &procedure, llvm::Function &fn) {
                size_t stride_loads = 0;
                for (llvm::BasicBlock &block: fn) {
                    for (llvm::Instruction &instruction: block) {
                        auto *load = llvm::dyn_cast<llvm::LoadInst>(&instruction);
                        if (!load) continue;
                        auto *gep = llvm::dyn_cast<llvm::GEPOperator>(
                            load->getPointerOperand());
                        if (!gep) continue;
                        for (auto it = llvm::gep_type_begin(*gep),
                                end = llvm::gep_type_end(*gep); it != end; ++it) {
                            llvm::StructType *type = it.getStructTypeOrNull();
                            if (!type || type->getName().str().rfind(
                                    "dimension_descriptor", 0) != 0) continue;
                            auto *field = llvm::dyn_cast<llvm::ConstantInt>(
                                it.getOperand());
                            if (field && field->equalsInt(2)) stride_loads++;
                        }
                    }
                }
                CAPTURE(fast);
                CAPTURE(procedure);
                CAPTURE(stride_loads);
                CHECK((stride_loads == 0) ==
                    (fast && procedure != "pointer"));
            }, false, false);
    }
}

TEST_CASE("LLVM descriptor entries with automatic BLOCK locals") {
    std::string source = R"(module block_entry_m
implicit none
type :: accumulator
    real :: total = 0
contains
    procedure :: automatic_block, saved_block, observer_block
end type
contains
subroutine automatic_block(self, x)
    class(accumulator), intent(inout) :: self
    real, intent(in) :: x(:)
    block
        real :: work(size(x))
        block
            work = 2*x
            self%total = sum(work)
        end block
    end block
end subroutine
subroutine saved_block(self, x)
    class(accumulator), intent(inout) :: self
    real, intent(in) :: x(:)
    block
        integer, save :: count = 0
        count = count + 1
        self%total = count + sum(x)
    end block
end subroutine
subroutine observer_block(self, x)
    class(accumulator), intent(inout) :: self
    real, intent(in) :: x(:)
    block
        if (is_contiguous(x)) then
            self%total = 1
        else
            self%total = -1
        end if
    end block
end subroutine
end module
)";
    for (bool fast: {false, true}) {
        for (bool stack_arrays: {false, true}) {
            inspect_definitions(source, fast,
                {{"automatic", "_m_automatic_block"},
                 {"saved", "_m_saved_block"},
                 {"observer", "_m_observer_block"}},
                [&](const std::string &procedure, llvm::Function &fn) {
                    size_t delegates = 0;
                    std::string suffix = "_m_" + procedure + "_block_real____1";
                    for (llvm::BasicBlock &block: fn) {
                        for (llvm::Instruction &instruction: block) {
                            auto *call = llvm::dyn_cast<llvm::CallInst>(&instruction);
                            if (!call || !call->getCalledFunction()) continue;
                            std::string name = call->getCalledFunction()->getName().str();
                            if (name.size() >= suffix.size() &&
                                    name.compare(name.size() - suffix.size(),
                                        suffix.size(), suffix) == 0) delegates++;
                        }
                    }
                    CAPTURE(fast);
                    CAPTURE(stack_arrays);
                    CAPTURE(procedure);
                    CAPTURE(delegates);
                    CHECK(delegates == (procedure == "automatic" ? 1 : 0));
                }, stack_arrays);
        }
    }
}

TEST_CASE("LLVM CLASS component storage provenance") {
    std::string source = R"(module component_storage_m
implicit none
type :: inner_t
    integer :: width
end type
type :: state_t
    integer :: width
    real :: fixed(4)
    real, allocatable :: values(:,:)
    real, allocatable :: scalar
    real, pointer :: alias(:)
    integer(kind=1) :: byte
    character :: text
    type(inner_t) :: inner
end type
type, extends(state_t) :: extended_t
end type
contains
subroutine touch(self, i, out)
    class(state_t), intent(inout) :: self
    integer, intent(in) :: i
    real, intent(out) :: out
    out = self%fixed(i) + self%values(i,1) + self%alias(i) + self%width
    self%fixed(i) = out
    self%values(i,1) = out
end subroutine
subroutine excluded_members(self, out)
    class(state_t), intent(in) :: self
    real, intent(out) :: out
    out = self%scalar + self%byte + iachar(self%text) + self%inner%width
end subroutine
subroutine parent_component(self, out)
    class(extended_t), intent(in) :: self
    real, intent(out) :: out
    out = self%state_t%width + self%state_t%values(1,1)
end subroutine
end module
)";
    auto procedures = std::vector<std::pair<std::string, std::string>>{
        {"touch", "_m_touch"}, {"excluded_members", "_m_excluded_members"},
        {"parent_component", "_m_parent_component"}};
    CHECK(tbaa_accesses(source, false, procedures).empty());
    inspect_definitions(source, true, procedures,
        [&](const std::string &procedure, llvm::Function &fn) {
            std::set<std::string> components;
            std::set<llvm::MDNode*> component_parents;
            llvm::MDNode *generic_data_type = nullptr;
            bool generic_pointer_data = false;
            for (llvm::BasicBlock &block: fn) {
                for (llvm::Instruction &inst: block) {
                    llvm::MDNode *tag = inst.getMetadata(
                        llvm::LLVMContext::MD_tbaa);
                    if (!tag) continue;
                    auto *type = llvm::cast<llvm::MDNode>(tag->getOperand(1));
                    std::string name = llvm::cast<llvm::MDString>(
                        type->getOperand(0))->getString().str();
                    if (name == "data") {
                        auto *load = llvm::dyn_cast<llvm::LoadInst>(&inst);
                        if (load && load->getType()->isFloatTy()) {
                            generic_pointer_data = true;
                            generic_data_type = type;
                        }
                    }
                    if (name != "inline component" &&
                            name != "allocated component") continue;
                    REQUIRE(procedure == "touch");
                    auto *parent = llvm::cast<llvm::MDNode>(type->getOperand(1));
                    component_parents.insert(parent);
                    CHECK(llvm::cast<llvm::MDString>(parent->getOperand(0))
                        ->getString() == "data");
                    auto *load = llvm::dyn_cast<llvm::LoadInst>(&inst);
                    auto *store = llvm::dyn_cast<llvm::StoreInst>(&inst);
                    REQUIRE((load || store));
                    llvm::Type *value_type = load ? load->getType()
                        : store->getValueOperand()->getType();
                    CHECK(!value_type->isPointerTy());
                    CHECK(!value_type->isIntegerTy(8));
                    components.insert(name);
                }
            }
            if (procedure == "touch") {
                std::set<std::string> expected = {
                    "inline component", "allocated component"};
                CHECK(components == expected);
                CHECK(generic_pointer_data);
                REQUIRE(component_parents.size() == 1);
                CHECK(*component_parents.begin() == generic_data_type);
            } else {
                CHECK(components.empty());
            }
        });

    // Exclusions change only the receiver, not the accesses under test.
    for (const char *declaration: {
            "type(state_t), intent(inout) :: self",
            "class(state_t), target, intent(inout) :: self",
            "class(state_t), pointer, intent(inout) :: self",
            "class(state_t), volatile, intent(inout) :: self",
            "class(state_t), asynchronous, intent(inout) :: self"}) {
        std::string excluded = source;
        std::string original = "class(state_t), intent(inout) :: self";
        excluded.replace(excluded.find(original), original.size(), declaration);
        std::string accesses = tbaa_accesses(excluded, true, {procedures[0]});
        CHECK(accesses.find("inline component") == std::string::npos);
        CHECK(accesses.find("allocated component") == std::string::npos);
    }
    // The declaration-scope query must see attributes in contained BLOCKs.
    std::string scoped = source;
    std::string start = "    out = self%fixed(i)";
    scoped.insert(scoped.find(start), "    block\n    volatile :: self\n");
    std::string end = "end subroutine";
    scoped.insert(scoped.find(end), "    end block\n");
    std::string accesses = tbaa_accesses(scoped, true, {procedures[0]});
    CHECK(accesses.find("inline component") == std::string::npos);
    CHECK(accesses.find("allocated component") == std::string::npos);
}

TEST_CASE("LLVM CLASS inline component load hoists across owned array writes") {
    std::string source = R"(module component_alias_m
implicit none
type :: state_t
    integer :: width
    real, allocatable :: values(:,:)
end type
contains
subroutine accumulate(self, x, n)
    class(state_t), intent(inout) :: self
    integer, intent(in) :: n
    real, intent(in) :: x(n,n)
    integer :: i, j
    do j = 1, n
        do i = 1, self%width
            self%values(i,j) = self%values(i,j) + x(i,j)
        end do
    end do
end subroutine
end module
)";
    inspect_definitions(source, true, {{"accumulate", "_m_accumulate"}},
        [&](const std::string &, llvm::Function &fn) {
            llvm::DominatorTree dominators(fn);
            llvm::LoopInfo loops(dominators);
            size_t width_reads = 0;
            size_t loop_stores = 0;
            for (llvm::BasicBlock &block: fn) {
                for (llvm::Instruction &inst: block) {
                    auto *store = llvm::dyn_cast<llvm::StoreInst>(&inst);
                    if (store && loops.getLoopFor(&block) &&
                            store->getValueOperand()->getType()->isFPOrFPVectorTy()) {
                        loop_stores++;
                    }
                    auto *load = llvm::dyn_cast<llvm::LoadInst>(&inst);
                    if (!load || !load->getType()->isIntegerTy(32)) continue;
                    llvm::MDNode *tag = load->getMetadata(
                        llvm::LLVMContext::MD_tbaa);
                    if (!tag) continue;
                    auto *type = llvm::cast<llvm::MDNode>(tag->getOperand(1));
                    auto *name = llvm::cast<llvm::MDString>(type->getOperand(0));
                    if (name->getString() == "inline component") {
                        width_reads++;
                        CHECK(loops.getLoopFor(load->getParent()) == nullptr);
                    }
                }
            }
            CHECK(width_reads == 1);
            CHECK(loop_stores > 0);
        }, false, true);
}

TEST_CASE("LLVM private array function result slots") {
    std::string source = R"(module private_result_m
implicit none
type :: operation_t
contains
    procedure :: evaluate
    procedure :: derivative
    procedure :: with_internal
end type
contains
pure function evaluate(self, x) result(res)
    class(operation_t), intent(in) :: self
    real, intent(in) :: x(:)
    real :: res(size(x))
    res = 2*x
end function
pure function derivative(self, x) result(res)
    class(operation_t), intent(in) :: self
    real, intent(in) :: x(:)
    real :: res(size(x))
    res = self%evaluate(x)*(1 - self%evaluate(x))
end function
pure function with_internal(self, x) result(res)
    class(operation_t), intent(in) :: self
    real, intent(in) :: x(:)
    real :: res(size(x))
    res = self%evaluate(x)*(1 - self%evaluate(x))
    if (size(x) > 0) res(1) = res(1) + probe()
contains
    pure function probe() result(value)
        real :: value
        value = sum(res)
    end function
end function
end module
)";
    for (bool fast: {false, true}) {
        inspect_definitions(source, fast,
            {{"derivative", "_m_derivative"},
             {"with_internal", "_m_with_internal"}},
            [&](const std::string &procedure, llvm::Function &fn) {
                size_t allocations = 0, evaluate_calls = 0;
                for (llvm::BasicBlock &block: fn) {
                    for (llvm::Instruction &instruction: block) {
                        auto *call = llvm::dyn_cast<llvm::CallInst>(&instruction);
                        if (!call) continue;
                        llvm::Function *callee = call->getCalledFunction();
                        if (!callee) evaluate_calls++;
                        if (callee && callee->getName() == "_lfortran_malloc_alloc") {
                            allocations++;
                        }
                    }
                }
                CAPTURE(fast);
                CAPTURE(procedure);
                CAPTURE(allocations);
                CHECK(evaluate_calls > 0);
                CHECK(allocations == evaluate_calls *
                    (procedure == "derivative" ? 1 : 2));
            });
    }
}

#if LLVM_VERSION_MAJOR >= 17
TEST_CASE("LLVM late cleanup hoists conditional window descriptors") {
    // Keep unswitching profitable without relying on target-specific unrolling.
    std::string source = R"(module conditional_window_updates
implicit none
type :: state_t
    integer :: width, channels, kernel_size, filters, stride
    real, allocatable :: kernel(:), gradient(:), dw(:,:,:)
contains
    procedure :: update
end type
contains
pure subroutine update(self, input, gradient, enabled)
    class(state_t), intent(inout) :: self
    real, intent(in) :: input(:,:), gradient(:,:)
    logical, intent(in) :: enabled
    real :: dw(self%filters,self%channels,self%kernel_size)
    integer :: i, k, n, r, iws, iwe
    dw = 0
    self%gradient = 0
    do i = 1, self%width
        iws = 1 + self%stride*(i-1)
        iwe = min(iws + self%kernel_size - 1, size(input,2))
        do k = 1, self%channels
            do n = 1, self%filters
                dw(n,k,1) = dw(n,k,1) + gradient(n,i)
                if (enabled) then
                    do r = iws, iwe
                        self%gradient(r) = self%gradient(r) + self%kernel(r-iws+1)
                    end do
                end if
            end do
        end do
    end do
    self%dw = self%dw + dw
end subroutine
end module
)";
    inspect_definitions(source, true,
        {{"update", "_update_real_______1_real_______2"}},
        [&](const std::string &, llvm::Function &fn) {
            llvm::DominatorTree dominators(fn);
            llvm::LoopInfo loops(dominators);
            struct Accesses {
                unsigned data_stores = 0, component_stores = 0, descriptor_loads = 0;
            };
            auto accesses = [](llvm::Loop *loop) {
                Accesses result;
                for (llvm::BasicBlock *block: loop->blocks()) {
                    for (llvm::Instruction &inst: *block) {
                        llvm::MDNode *tag = inst.getMetadata(llvm::LLVMContext::MD_tbaa);
                        if (!tag) continue;
                        auto *type = llvm::cast<llvm::MDNode>(tag->getOperand(1));
                        auto *name = llvm::cast<llvm::MDString>(type->getOperand(0));
                        if (auto *store = llvm::dyn_cast<llvm::StoreInst>(&inst)) {
                            if (!store->getValueOperand()->getType()->isFPOrFPVectorTy()) continue;
                            result.data_stores += name->getString() == "data";
                            result.component_stores += name->getString() == "allocated component";
                        } else if (llvm::isa<llvm::LoadInst>(inst)) {
                            result.descriptor_loads += name->getString() == "descriptor";
                        }
                    }
                }
                return result;
            };
            std::vector<llvm::Loop *> pending(loops.begin(), loops.end());
            // Versioning and unrolling may clone stores and update loops.
            unsigned update_loops = 0;
            while (!pending.empty()) {
                llvm::Loop *loop = pending.back();
                pending.pop_back();
                pending.insert(pending.end(), loop->begin(), loop->end());
                Accesses found = accesses(loop);
                if (!found.data_stores || !found.component_stores) continue;
                bool nested_update = false;
                for (llvm::Loop *child: *loop) {
                    Accesses nested = accesses(child);
                    nested_update |= nested.data_stores && nested.component_stores;
                }
                if (nested_update) continue;
                update_loops++;
                CHECK(found.descriptor_loads == 0);
                REQUIRE(loop->getParentLoop() != nullptr);
#if LLVM_VERSION_MAJOR >= 18
                // LLVM 17's non-trivial unswitching leaves these descriptor
                // loads guarded inside the parent loop, so LICM cannot hoist
                // them.
                CHECK(accesses(loop->getParentLoop()).descriptor_loads == 0);
#endif
            }
            CHECK(update_loops > 0);
        }, false, true);
}
#endif

TEST_CASE("LLVM static SAVE initialization with VOLATILE in a BLOCK") {
    std::string source = R"(module saved_volatile_m
implicit none
contains
subroutine accumulate(result)
    integer, intent(out) :: result
    block
        integer, save :: counter = 7
        volatile :: counter
        counter = counter + 1
        result = counter
    end block
end subroutine
end module
)";
    for (bool fast: {false, true}) {
        inspect_definitions(source, fast, {{"accumulate", "_m_accumulate"}},
            [&](const std::string &, llvm::Function &fn) {
                unsigned saved_stores = 0;
                unsigned saved_loads = 0;
                for (llvm::BasicBlock &block: fn) {
                    for (llvm::Instruction &inst: block) {
                        if (auto *store = llvm::dyn_cast<llvm::StoreInst>(&inst)) {
                            auto *global = llvm::dyn_cast<llvm::GlobalVariable>(
                                store->getPointerOperand());
                            if (!global) continue;
                            auto *initial = llvm::dyn_cast<llvm::ConstantInt>(
                                global->getInitializer());
                            REQUIRE(initial != nullptr);
                            CHECK(initial->getSExtValue() == 7);
                            CHECK(!llvm::isa<llvm::Constant>(store->getValueOperand()));
                            CHECK(store->getMetadata(llvm::LLVMContext::MD_tbaa)
                                == nullptr);
                            saved_stores++;
                        } else if (auto *load = llvm::dyn_cast<llvm::LoadInst>(&inst)) {
                            if (!llvm::isa<llvm::GlobalVariable>(
                                    load->getPointerOperand())) continue;
                            CHECK(load->getMetadata(llvm::LLVMContext::MD_tbaa)
                                == nullptr);
                            saved_loads++;
                        }
                    }
                }
                CHECK(saved_stores == 1);
                CHECK(saved_loads == 2);
            });
    }
}

TEST_CASE("LLVM conditional constructor temporaries allocate only in their branch") {
    std::string source = R"(module constructor_scope_ir_m
implicit none
type :: base
    real :: learning_rate = 0.25
end type
type, extends(base) :: optimizer
    real :: momentum = 0
    real, allocatable :: velocity(:)
end type
type :: finalizable
    real, allocatable :: values(:)
contains
    final :: finish
end type
type :: pointer_owner
    real, allocatable :: values(:)
    real, pointer :: link => null()
end type
contains
subroutine initialize(slot, enabled)
    class(base), allocatable, intent(inout) :: slot
    logical, intent(in) :: enabled
    if (enabled) slot = optimizer()
end subroutine
subroutine initialize_finalizable(slot, enabled)
    class(finalizable), allocatable, intent(inout) :: slot
    logical, intent(in) :: enabled
    if (enabled) slot = finalizable()
end subroutine
subroutine initialize_pointer(slot, enabled)
    class(pointer_owner), allocatable, intent(inout) :: slot
    logical, intent(in) :: enabled
    if (enabled) slot = pointer_owner()
end subroutine
subroutine finish(self)
    type(finalizable), intent(inout) :: self
end subroutine
end module
)";
    for (bool fast: {false, true}) {
        inspect_definitions(source, fast,
            {{"initialize", "_m_initialize"},
             {"initialize_finalizable", "_m_initialize_finalizable"},
             {"initialize_pointer", "_m_initialize_pointer"}},
            [&](const std::string &procedure, llvm::Function &fn) {
                unsigned entry_allocations = 0;
                for (llvm::Instruction &inst: fn.getEntryBlock()) {
                    auto *call = llvm::dyn_cast<llvm::CallInst>(&inst);
                    if (call && call->getCalledFunction() &&
                            call->getCalledFunction()->getName() ==
                                "_lfortran_malloc_alloc") {
                        entry_allocations++;
                    }
                }
                CHECK(entry_allocations == (procedure == "initialize" ? 0 : 1));
            });
    }
}

// The runtime allocation functions whose declarations in the module of the
// LLVM definition of `procedure` have a noalias return value, as
// "name name ...".
std::string noalias_allocators(const std::string &source, bool fast,
        const std::pair<std::string, std::string> &procedure) {
    std::string result;
    inspect_definitions(source, fast, {procedure},
        [&](const std::string &, llvm::Function &fn) {
            for (const char *name: {"_lfortran_malloc_alloc",
                    "_lfortran_realloc_alloc",
                    "_lfortran_get_default_allocator"}) {
                llvm::Function *f = fn.getParent()->getFunction(name);
                if (f != nullptr && f->returnDoesNotAlias()) {
                    result += std::string(name) + " ";
                }
            }
        });
    return result;
}

TEST_CASE("LLVM TBAA of local arrays and noalias allocations under --fast") {
    std::string source = R"(module tbaa_local_ir_m
use iso_c_binding, only: c_loc, c_ptr
implicit none
contains

subroutine automatic(x, n, s)
    integer, intent(in) :: n
    real, intent(in) :: x(n)
    real, intent(out) :: s
    real :: a(n)
    integer :: i
    s = 0
    do i = 1, n
        a(i) = 2*x(i)
        s = s + a(i)
    end do
end subroutine

subroutine fixed(s)
    integer, intent(out) :: s
    integer :: k(8)
    integer :: i
    s = 0
    do i = 1, 8
        k(i) = i
        s = s + k(i)
    end do
end subroutine

subroutine equivalenced(s)
    integer, intent(out) :: s
    real :: r(8)
    integer :: k(8)
    equivalence (r, k)
    integer :: i
    s = 0
    do i = 1, 8
        k(i) = i
        s = s + k(i)
    end do
end subroutine

subroutine in_common(s)
    integer, intent(out) :: s
    integer :: k(8)
    common /tbaa_local_blk/ k
    integer :: i
    s = 0
    do i = 1, 8
        k(i) = i
        s = s + k(i)
    end do
end subroutine

subroutine address_taken(n, s, p)
    integer, intent(in) :: n
    integer, intent(out) :: s
    type(c_ptr), intent(out) :: p
    integer, target :: k(n)
    integer :: i
    s = 0
    do i = 1, n
        k(i) = i
        s = s + k(i)
    end do
    p = c_loc(k)
end subroutine

subroutine bind_c(n, s) bind(c)
    integer, value :: n
    integer, intent(out) :: s
    integer :: k(n)
    integer :: i
    s = 0
    do i = 1, n
        k(i) = i
        s = s + k(i)
    end do
end subroutine

subroutine volatile_local(s)
    integer, intent(out) :: s
    integer, volatile :: k(8)
    integer :: i
    s = 0
    do i = 1, 8
        k(i) = i
        s = s + k(i)
    end do
end subroutine

subroutine asynchronous_local(s)
    integer, intent(out) :: s
    integer, asynchronous :: k(8)
    integer :: i
    s = 0
    do i = 1, 8
        k(i) = i
        s = s + k(i)
    end do
end subroutine

subroutine saved_local(s)
    integer, intent(out) :: s
    integer, save :: k(8)
    integer :: i
    s = 0
    do i = 1, 8
        k(i) = i
        s = s + k(i)
    end do
end subroutine

end module
)";
    std::vector<std::pair<std::string, std::string>> procedures = {
        {"automatic", "_m_automatic"},
        {"fixed", "_m_fixed"},
        {"equivalenced", "_m_equivalenced"},
        {"in_common", "_m_in_common"},
        {"address_taken", "_m_address_taken"},
        {"bind_c", "bind_c"},
        {"volatile_local", "_m_volatile_local"},
        {"asynchronous_local", "_m_asynchronous_local"},
        {"saved_local", "_m_saved_local"}};
    // The elements of the automatic array a and of the fixed-size array k
    // are data. The elements of an array of EQUIVALENCE (k, the storage of
    // r), of a common block or whose address C_LOC takes are not tagged,
    // nor is anything in a bind(c) procedure. In equivalenced only the
    // setup of the descriptor of r, a pointer to k, and its independent
    // local shape array are tagged.
    std::string expected =
        "automatic:load float:data; "
        "automatic:store float:data; "
        "equivalenced:store i32:data; "
        "equivalenced:store i64:descriptor; "
        "equivalenced:store i8:descriptor; "
        "equivalenced:store pointer:descriptor; "
        "fixed:load i32:data; "
        "fixed:store i32:data; ";
    std::string expected_shape_load = expected;
    expected_shape_load.insert(expected_shape_load.find("equivalenced:store"),
        "equivalenced:load i32:data; ");
    std::string accesses = tbaa_accesses(source, true, procedures);
    CHECK((accesses == expected || accesses == expected_shape_load));
    CHECK(tbaa_accesses(source, false, procedures).empty());
    // EQUIVALENCE also creates an independent local shape array [8].
    // With opaque pointers its extent load needs no bitcast and may have
    // data TBAA. Check storage provenance, not a procedure-wide load type.
    for (bool fast: {false, true}) {
        inspect_definitions(source, fast, {{"equivalenced", "_m_equivalenced"}},
            [&](const std::string &, llvm::Function &fn) {
                unsigned aliased_loads = 0, aliased_stores = 0;
                unsigned shape_loads = 0, shape_stores = 0;
                for (llvm::BasicBlock &block: fn) {
                    for (llvm::Instruction &inst: block) {
                        llvm::Value *ptr = nullptr;
                        auto *load = llvm::dyn_cast<llvm::LoadInst>(&inst);
                        auto *store = llvm::dyn_cast<llvm::StoreInst>(&inst);
                        if (load) ptr = load->getPointerOperand();
                        if (store) ptr = store->getPointerOperand();
                        if (!ptr) continue;
                        llvm::MDNode *tag = inst.getMetadata(
                            llvm::LLVMContext::MD_tbaa);
                        if (!fast) CHECK(tag == nullptr);
                        ptr = ptr->stripPointerCasts();
                        while (auto *gep =
                                llvm::dyn_cast<llvm::GetElementPtrInst>(ptr)) {
                            ptr = gep->getPointerOperand()->stripPointerCasts();
                        }
                        auto *storage = llvm::dyn_cast<llvm::AllocaInst>(ptr);
                        if (tag && load && load->getType()->isIntegerTy(32)) {
                            REQUIRE(storage != nullptr);
                            CHECK(storage->getAllocatedType() ==
                                llvm::ArrayType::get(load->getType(), 1));
                        }
                        if (!storage) continue;
                        auto *array = llvm::dyn_cast<llvm::ArrayType>(
                            storage->getAllocatedType());
                        if (!array) continue;
                        REQUIRE(array->getElementType()->isIntegerTy(32));
                        if (storage->getName() == "k") {
                            CHECK(array->getNumElements() == 8);
                            CHECK(tag == nullptr);
                            if (load) aliased_loads++;
                            if (store) aliased_stores++;
                        } else {
                            REQUIRE(array->getNumElements() == 1);
                            if (load) shape_loads++;
                            if (store) {
                                auto *extent = llvm::dyn_cast<llvm::ConstantInt>(
                                    store->getValueOperand());
                                REQUIRE(extent != nullptr);
                                CHECK(extent->getSExtValue() == 8);
                                CHECK((tag != nullptr) == fast);
                                shape_stores++;
                            }
                        }
                    }
                }
                CHECK(aliased_loads == 1);
                CHECK(aliased_stores == 1);
                CHECK(shape_loads == 1);
                CHECK(shape_stores == 1);
            });
    }
    // The storage of an automatic array comes from _lfortran_malloc_alloc,
    // which returns fresh memory; realloc may return its argument.
    CHECK(noalias_allocators(source, true, procedures[0]) ==
        "_lfortran_malloc_alloc ");
    CHECK(noalias_allocators(source, false, procedures[0]).empty());
}

TEST_CASE("LLVM allocator accessors under --fast") {
    const std::string source = R"(module allocator_accessors_ir_m
implicit none
contains
subroutine unused()
    real, allocatable :: a(:), b(:)
end subroutine

subroutine needed(n)
    integer, intent(in) :: n
    real, allocatable :: a(:), b(:)
    interface
        subroutine consume(a, b, n)
            integer, intent(in) :: n
            real, intent(inout) :: a(n), b(n)
        end subroutine
    end interface
    allocate(a(n), b(n))
    call consume(a, b, n)
    deallocate(a, b)
end subroutine
end module)";
    for (bool detect_leaks: {false, true}) {
        const char *getter = detect_leaks
            ? "_lfortran_get_compiler_mem_dbg_allocator"
            : "_lfortran_get_default_allocator";
        for (bool fast: {false, true}) {
            inspect_definitions(source, fast, {{"needed", "needed"}},
                [&](const std::string &, llvm::Function &fn) {
                    llvm::Function *f = fn.getParent()->getFunction(getter);
                    REQUIRE(f != nullptr);
                    CHECK(f->doesNotAccessMemory() == fast);
                    CHECK(f->doesNotThrow() == fast);
#if LLVM_VERSION_MAJOR >= 9
                    CHECK(f->hasFnAttribute(llvm::Attribute::WillReturn) == fast);
#endif
                    CHECK(!f->returnDoesNotAlias());
                    for (const char *name: {"_lfortran_malloc_alloc",
                            "_lfortran_free_alloc"}) {
                        llvm::Function *allocation =
                            fn.getParent()->getFunction(name);
                        REQUIRE(allocation != nullptr);
                        CHECK(!allocation->onlyReadsMemory());
                    }
                }, false, false, detect_leaks);
        }
        inspect_definitions(source, true,
            {{"unused", "unused"}, {"needed", "needed"}},
            [&](const std::string &procedure, llvm::Function &fn) {
                int getters = 0, allocations = 0;
                llvm::Value *identity = nullptr;
                for (llvm::BasicBlock &bb: fn) {
                    for (llvm::Instruction &inst: bb) {
                        auto *call = llvm::dyn_cast<llvm::CallInst>(&inst);
                        if (!call || !call->getCalledFunction()) continue;
                        llvm::StringRef name = call->getCalledFunction()->getName();
                        if (name == getter) {
                            getters++;
                        } else if (name == "_lfortran_malloc_alloc" ||
                                name == "_lfortran_free_alloc") {
                            llvm::Value *allocator = call->getArgOperand(0);
                            if (identity) CHECK(allocator == identity);
                            identity = allocator;
                            allocations++;
                        }
                    }
                }
                if (procedure == "unused") {
                    CHECK(getters == 0);
                    CHECK(allocations == 0);
                } else {
                    CHECK(getters == 1);
                    CHECK(allocations >= 4);
                }
            }, false, true, detect_leaks);
    }
}

TEST_CASE("LLVM conditional allocation keeps allocator access on slow paths") {
    const std::string source = R"(module lazy_allocator_ir_m
implicit none
contains
subroutine conditional(needed, n, work)
    logical, intent(in) :: needed
    integer, intent(in) :: n
    real, allocatable, intent(out) :: work(:)
    if (needed) allocate(work(n))
end subroutine
end module)";
    for (bool detect_leaks: {false, true}) {
        const char *getter = detect_leaks
            ? "_lfortran_get_compiler_mem_dbg_allocator"
            : "_lfortran_get_default_allocator";
        for (int mode = 0; mode < 6; mode++) {
            bool fast = mode != 0;
            bool lazy = mode == 1;
            for (bool optimize: {false, true}) {
                CAPTURE(detect_leaks);
                CAPTURE(mode);
                CAPTURE(optimize);
                // With a GPU device the body is in the deferring twin of
                // `conditional` (gpu_launch_sync.cpp).
                std::string twin = mode == 4 || mode == 5
                    ? "conditional__gpu_deferred" : "conditional";
                inspect_definitions(source, fast, {{"conditional", twin}},
                    [&](const std::string &, llvm::Function &fn) {
                        int getters = 0, entry_getters = 0;
                        int allocations = 0, frees = 0;
                        std::set<llvm::BasicBlock *> allocator_blocks;
                        for (llvm::BasicBlock &bb: fn) {
                            for (llvm::Instruction &inst: bb) {
                                auto *call = llvm::dyn_cast<llvm::CallInst>(&inst);
                                if (!call || !call->getCalledFunction()) continue;
                                llvm::StringRef name = call->getCalledFunction()->getName();
                                if (name == getter) {
                                    getters++;
                                    if (&bb == &fn.getEntryBlock()) entry_getters++;
                                } else if (name == "_lfortran_malloc_alloc") {
                                    allocations++;
                                } else if (name == "_lfortran_free_alloc") {
                                    frees++;
                                }
                                if (name == getter ||
                                        name == "_lfortran_malloc_alloc" ||
                                        name == "_lfortran_free_alloc") {
                                    allocator_blocks.insert(&bb);
                                }
                            }
                        }
                        CHECK(getters >= 1);
                        CHECK(allocations >= 1);
                        CHECK(frees >= 1);
                        CHECK(entry_getters == (lazy ? 0 : 1));
                        if (lazy) {
                            std::vector<llvm::BasicBlock *> pending{&fn.getEntryBlock()};
                            std::set<llvm::BasicBlock *> visited;
                            bool allocation_free_return = false;
                            while (!pending.empty()) {
                                llvm::BasicBlock *bb = pending.back();
                                pending.pop_back();
                                if (!visited.insert(bb).second ||
                                        allocator_blocks.count(bb)) continue;
                                llvm::Instruction *end = bb->getTerminator();
                                if (llvm::isa<llvm::ReturnInst>(end)) {
                                    allocation_free_return = true;
                                }
                                for (llvm::BasicBlock *successor: llvm::successors(bb)) {
                                    pending.push_back(successor);
                                }
                            }
                            CHECK(allocation_free_return);
                        }
                    }, false, optimize, detect_leaks, [=](CompilerOptions &co) {
                        co.openmp = mode == 2;
                        co.po.coarray = mode == 3;
                        co.po.gpu_offload_metal = mode == 4;
                        co.po.gpu_offload_cuda = mode == 5;
                    });
            }
        }
    }
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

TEST_CASE("c_long_double is the kind of C long double of the target") {
    // The values of __LDBL_MANT_DIG__ in clang: 64 -> 10, 53 -> 8, 113 -> 16
    std::vector<std::pair<std::string, int>> triples = {
        {"x86_64-unknown-linux-gnu", 10},
        {"x86_64-linux-gnu", 10},
        {"x86_64-apple-darwin", 10},
        {"x86_64h-apple-darwin", 10},
        {"i686-pc-linux-gnu", 10},
        {"i686-w64-windows-gnu", 10},
        {"i686-w64-mingw32", 10},
        {"i686-pc-cygwin", 10},
        {"i686-pc-win32", 8},
        {"x86_64-w64-mingw32", 10},
        {"x86_64-pc-windows-gnu", 10},
        {"x86_64-pc-cygwin", 10},
        {"x86_64-pc-windows-cygnus", 10},
        {"x86_64-pc-windows-msvc", 8},
        {"x86_64-pc-windows-msvc19.30.0", 8},
        {"x86_64-pc-win32", 8},
        {"x86_64-pc-windows", 8},
        {"x86_64-windows", 8},
        {"x86_64-pc-windows-itanium", 8},
        {"i686-linux-android", 8},
        {"x86_64-linux-android", 16},
        {"aarch64-unknown-linux-gnu", 16},
        {"aarch64-linux-android", 16},
        {"aarch64_be-unknown-linux-gnu", 16},
        {"aarch64-apple-darwin", 8},
        {"arm64-apple-macosx14.0.0", 8},
        {"arm64e-apple-ios", 8},
        {"arm64_32-apple-watchos", 8},
        {"aarch64-pc-windows-msvc", 8},
        {"aarch64-pc-win32", 8},
        {"aarch64-w64-mingw32", 8},
        {"aarch64-pc-windows-gnu", 8},
        {"arm64ec-pc-windows-msvc", 8},
        {"wasm32-wasi", 16},
        {"wasm32-unknown-emscripten", 16},
        {"wasm64-unknown-unknown", 16},
    };
    for (auto &t : triples) {
        INFO(t.first);
        CHECK(LCompilers::get_c_long_double_kind(t.first) == t.second);
    }

    // The intrinsic module gives the kind of the target it is used for
    for (int kind : {8, 10, 16}) {
        CompilerOptions cu;
        cu.interactive = true;
        cu.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
        cu.po.c_long_double_kind = kind;
        FortranEvaluator e(cu);
        LCompilers::Result<FortranEvaluator::EvalResult>
        r = e.evaluate2("use iso_c_binding, only: ld => c_long_double, c_long_double_complex");
        CHECK(r.ok);
        r = e.evaluate2("ld");
        CHECK(r.ok);
        CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
        CHECK(r.result.i32 == kind);
        r = e.evaluate2("c_long_double_complex");
        CHECK(r.ok);
        CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
        CHECK(r.result.i32 == (kind == 8 ? 8 : -1));
    }
}

TEST_CASE("iso_c_binding pointer-sized kinds of the target") {
    const std::vector<std::pair<std::string, int>> triples = {
        {"x86_64-unknown-linux-gnu", 8},
        {"x86_64-linux-gnux32", 4},
        {"i686-pc-linux-gnu", 4},
        {"aarch64-apple-darwin", 8},
        {"arm64_32-apple-watchos", 4},
        {"armv7-unknown-linux-gnueabihf", 4},
        {"thumbv7em-none-eabi", 4},
        {"x86_64-pc-windows-msvc", 8},
        {"riscv32-unknown-elf", 4},
        {"riscv64-unknown-linux-gnu", 8},
        {"wasm32-unknown-emscripten", 4},
        {"wasm32-wasi", 4},
        {"wasm64-unknown-unknown", 8},
    };
    for (auto &t : triples) {
        INFO(t.first);
        CHECK(LCompilers::get_c_pointer_kind(t.first) == t.second);
    }

    // The intrinsic module gives the sizes of the target it is used for; on
    // a 32-bit target `long` is as wide as a pointer
    for (int kind : {4, 8}) {
        CompilerOptions cu;
        cu.interactive = true;
        cu.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
        cu.po.c_pointer_kind = kind;
        FortranEvaluator e(cu);
        LCompilers::Result<FortranEvaluator::EvalResult>
        r = e.evaluate2("use iso_c_binding, only: c_size_t, c_intptr_t, c_ptrdiff_t, c_long");
        CHECK(r.ok);
        for (const char *name : {"c_size_t", "c_intptr_t", "c_ptrdiff_t"}) {
            INFO(name);
            r = e.evaluate2(name);
            CHECK(r.ok);
            CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
            CHECK(r.result.i32 == kind);
        }
        r = e.evaluate2("c_long");
        CHECK(r.ok);
        CHECK(r.result.i32 == kind);
    }
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

// A derived-type global declared with a structure constructor keeps the
// constructor's component values. The members are set up at run time in
// interactive mode, and that setup must not fall back to the components'
// declared defaults and overwrite the initializer (#16859).
TEST_CASE("FortranEvaluator derived type constructor initializer") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);

    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("type :: init_point\ninteger :: x, y\nend type");
    CHECK(r.ok);
    r = e.evaluate2("type(init_point) :: init_p = init_point(1, 2)");
    CHECK(r.ok);
    r = e.evaluate2("init_p%x");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 1);
    r = e.evaluate2("init_p%y");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 2);
}

// A component inherited from a parent type declared in an earlier cell than
// the extended type, read through a variable declared in yet another cell.
TEST_CASE("FortranEvaluator parent component of an extended type from earlier cells") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);

    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("type :: point_16952\ninteger :: x, y\nend type");
    REQUIRE(r.ok);
    r = e.evaluate2(R"(type, extends(point_16952) :: p3_16952
integer :: z = 9
end type
)");
    REQUIRE(r.ok);
    r = e.evaluate2("type(p3_16952) :: q_16952");
    REQUIRE(r.ok);
    r = e.evaluate2("q_16952%z");
    REQUIRE(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 9);
    r = e.evaluate2("q_16952%x = 4\nq_16952%y = 5");
    REQUIRE(r.ok);
    r = e.evaluate2("q_16952%x");
    REQUIRE(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 4);
    r = e.evaluate2("q_16952%x + q_16952%y");
    REQUIRE(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 9);
}

// The same, with the type and the declaration in one cell and the use in
// another.
TEST_CASE("FortranEvaluator derived type initializer in one cell") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);

    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2(R"(type :: init_cell_point
integer :: x, y
end type
type(init_cell_point) :: init_cell_p = init_cell_point(5, 6))");
    CHECK(r.ok);
    r = e.evaluate2("init_cell_p%x");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 5);
    r = e.evaluate2("init_cell_p%y");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 6);

    // The type, the declaration and a use of the result in a single cell.
    r = e.evaluate2(R"(type :: init_use_point
integer :: x, y
end type
type(init_use_point) :: init_use_p = init_use_point(7, 8)
init_use_p%y)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 8);
}

// Controls for the same cell layout: a component's own default initializer
// and a later assignment must keep working.
TEST_CASE("FortranEvaluator derived type initializer controls") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);

    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("type :: init_ctrl_point\ninteger :: x = 3, y = 4\nend type");
    CHECK(r.ok);
    r = e.evaluate2("type(init_ctrl_point) :: init_ctrl_default");
    CHECK(r.ok);
    r = e.evaluate2("init_ctrl_default%x");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 3);

    r = e.evaluate2("type(init_ctrl_point) :: init_ctrl_assigned");
    CHECK(r.ok);
    r = e.evaluate2("init_ctrl_assigned = init_ctrl_point(9, 10)");
    CHECK(r.ok);
    r = e.evaluate2("init_ctrl_assigned%x");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 9);
    r = e.evaluate2("init_ctrl_assigned%y");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 10);
}

// The member setup of an interactive global must also apply a structure
// constructor's string and nested derived-type component values (#16859).
TEST_CASE("FortranEvaluator derived type initializer string and nested components") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);

    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("type :: init_string_point\ncharacter(len=3) :: s\nend type");
    CHECK(r.ok);
    r = e.evaluate2("type(init_string_point) :: init_string_p = init_string_point('abc')");
    CHECK(r.ok);
    r = e.evaluate2("init_string_p%s");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::character);
    CHECK(r.result.str == "abc");

    r = e.evaluate2("type :: init_inner_point\ninteger :: k\nend type");
    CHECK(r.ok);
    r = e.evaluate2("type :: init_nested_point\ntype(init_inner_point) :: inner\nend type");
    CHECK(r.ok);
    r = e.evaluate2("type(init_nested_point) :: init_nested_p = init_nested_point(init_inner_point(11))");
    CHECK(r.ok);
    r = e.evaluate2("init_nested_p%inner%k");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 11);
}

// A derived-type named constant declared in an earlier cell initializes a
// variable of the same type in a later cell. The value of the constant names
// the type it was constructed with, which must be the same type a later cell
// declares from (#16953).
TEST_CASE("FortranEvaluator derived type parameter from an earlier cell") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);

    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("type :: param_point\ninteger :: x, y\nend type");
    CHECK(r.ok);
    r = e.evaluate2("type(param_point), parameter :: param_cp = param_point(7, 8)");
    CHECK(r.ok);
    r = e.evaluate2("type(param_point) :: param_pc = param_cp");
    CHECK(r.ok);
    r = e.evaluate2("param_pc%x");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 7);
    r = e.evaluate2("param_pc%y");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 8);

    // A constant whose value has a component of another derived type.
    r = e.evaluate2("type :: param_inner\ninteger :: k\nend type");
    CHECK(r.ok);
    r = e.evaluate2("type :: param_outer\ntype(param_inner) :: inner\nend type");
    CHECK(r.ok);
    r = e.evaluate2("type(param_outer), parameter :: param_co = param_outer(param_inner(12))");
    CHECK(r.ok);
    r = e.evaluate2("type(param_outer) :: param_po = param_co");
    CHECK(r.ok);
    r = e.evaluate2("param_po%inner%k");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 12);
}

// The same with the type, the constant and the variable in one cell.
TEST_CASE("FortranEvaluator derived type parameter in one cell") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);

    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2(R"(type :: param_cell_point
integer :: x, y
end type
type(param_cell_point), parameter :: param_cell_cp = param_cell_point(7, 8)
type(param_cell_point) :: param_cell_pc = param_cell_cp)");
    CHECK(r.ok);
    r = e.evaluate2("param_cell_pc%x * 10 + param_cell_pc%y");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 78);
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

// A fixed-size array a cell declares with an initializer is initialized
// statically, like a module variable (#13459).
TEST_CASE("FortranEvaluator Array initializer") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("integer :: a(3) = [1, 2, 3]");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::none);
    r = e.evaluate2("a(3)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 3);
    r = e.evaluate2("sum(a)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 6);

    r = e.evaluate2("integer :: b(3) = (/4, 5, 6/)");
    CHECK(r.ok);
    r = e.evaluate2("b(2)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 5);

    r = e.evaluate2("integer :: c(4) = 7");
    CHECK(r.ok);
    r = e.evaluate2("sum(c)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 28);

    r = e.evaluate2("real(8) :: d(2) = [1.5d0, 2.5d0]");
    CHECK(r.ok);
    r = e.evaluate2("d(1) + d(2)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::real8);
    CHECK(r.result.f64 == 4.0);

    r = e.evaluate2("character(len=2) :: s(2) = [\"ab\", \"cd\"]\ns(1) == \"ab\" .and. s(2) == \"cd\"");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::boolean);
    CHECK(r.result.b);

    // The array stays assignable after its static initialization.
    r = e.evaluate2("a(1) = 10");
    CHECK(r.ok);
    r = e.evaluate2("sum(a)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 15);

    // The declaration and a use of the array in a single cell.
    r = e.evaluate2("integer :: f(2) = [8, 9]\nf(1) * f(2)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 72);
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

TEST_CASE("FortranEvaluator independent session symbol ownership") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    {
        FortranEvaluator functions(cu);
        REQUIRE(functions.evaluate2(R"(integer function session_symbol()
session_symbol = 17
end function
)").ok);
        auto r = functions.evaluate2("session_symbol()");
        REQUIRE(r.ok);
        CHECK(r.result.i32 == 17);
        {
            FortranEvaluator variables(cu);
            REQUIRE(variables.evaluate2(
                "integer, allocatable :: session_symbol").ok);
            r = variables.evaluate2("allocated(session_symbol)");
            REQUIRE(r.ok);
            REQUIRE(r.result.type == FortranEvaluator::EvalResult::boolean);
            REQUIRE_FALSE(r.result.b);
            REQUIRE(variables.evaluate2(
                "allocate(session_symbol, source=7)").ok);
            r = variables.evaluate2("session_symbol + 0");
            REQUIRE(r.ok);
            CHECK(r.result.i32 == 7);
            REQUIRE(variables.evaluate2("deallocate(session_symbol)").ok);
            r = variables.evaluate2("allocated(session_symbol)");
            REQUIRE(r.ok);
            CHECK_FALSE(r.result.b);
        }
        r = functions.evaluate2("session_symbol()");
        REQUIRE(r.ok);
        CHECK(r.result.i32 == 17);
    }
    {
        FortranEvaluator functions(cu);
        REQUIRE(functions.evaluate2(R"(integer function session_symbol()
session_symbol = 29
end function
)").ok);
        auto r = functions.evaluate2("session_symbol()");
        REQUIRE(r.ok);
        CHECK(r.result.i32 == 29);
    }
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

TEST_CASE("FortranEvaluator interface body to an earlier cell's function") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    // The first cell emits unqualified symbols, so the callee is declared
    // by a later one, which qualifies them by its cell.
    REQUIRE(e.evaluate2("integer :: z0").ok);
    REQUIRE(e.evaluate2(R"(integer function fn()
fn = 1
end function
)").ok);
    REQUIRE(e.evaluate2(R"(integer function call_fn()
    interface
        integer function fn()
        end function
    end interface
    call_fn = fn()
end function
)").ok);
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2("call_fn()");
    REQUIRE(r.ok);
    CHECK(r.result.i32 == 1);

    // The interface binds to the `fn` in scope when it is compiled: a later
    // `fn` shadows it for new code only.
    REQUIRE(e.evaluate2(R"(integer function fn()
fn = 2
end function
)").ok);
    r = e.evaluate2("call_fn()");
    REQUIRE(r.ok);
    CHECK(r.result.i32 == 1);
    REQUIRE(e.evaluate2(R"(integer function call_fn2()
    interface
        integer function fn()
        end function
    end interface
    call_fn2 = fn()
end function
)").ok);
    r = e.evaluate2("call_fn2()");
    REQUIRE(r.ok);
    CHECK(r.result.i32 == 2);
}

TEST_CASE("FortranEvaluator interface body to an earlier cell's array function") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    REQUIRE(e.evaluate2("integer :: dummy_d").ok);
    REQUIRE(e.evaluate2(R"(real(8) function ext_d(x)
real(8), intent(in) :: x(:)
ext_d = sum(x) * 2
end function
)").ok);
    REQUIRE(e.evaluate2(R"(subroutine host_d(r)
real(8), intent(out) :: r
interface
real(8) function ext_d(x)
real(8), intent(in) :: x(:)
end function
end interface
real(8) :: a(3)
a = [1.0d0, 2.0d0, 3.0d0]
r = ext_d(a)
end subroutine
)").ok);
    REQUIRE(e.evaluate2("real(8) :: rr_d").ok);
    REQUIRE(e.evaluate2("call host_d(rr_d)").ok);
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2("rr_d");
    REQUIRE(r.ok);
    CHECK(r.result.f64 == 12.0);
}

TEST_CASE("FortranEvaluator interface body to an earlier cell's function as actual argument") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    REQUIRE(e.evaluate2("integer :: z0").ok);
    REQUIRE(e.evaluate2(R"(real function ext_real(x)
real, intent(in) :: x
ext_real = 3 * x
end function
)").ok);
    REQUIRE(e.evaluate2(R"(real function apply(f, x)
interface
real function f(x)
real, intent(in) :: x
end function
end interface
real, intent(in) :: x
apply = f(x)
end function
)").ok);
    REQUIRE(e.evaluate2(R"(real function host_arg()
interface
real function ext_real(x)
real, intent(in) :: x
end function
real function apply(f, x)
interface
real function f(x)
real, intent(in) :: x
end function
end interface
real, intent(in) :: x
end function
end interface
host_arg = apply(ext_real, 2.0)
end function
)").ok);
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2("host_arg()");
    REQUIRE(r.ok);
    CHECK(r.result.f32 == 6.0f);
}

TEST_CASE("FortranEvaluator interface body to a function of the same cell") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    REQUIRE(e.evaluate2("integer :: z0").ok);
    REQUIRE(e.evaluate2(R"(integer function fn()
fn = 5
end function

integer function call_fn()
    interface
        integer function fn()
        end function
    end interface
    call_fn = fn()
end function
)").ok);
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2("call_fn()");
    REQUIRE(r.ok);
    CHECK(r.result.i32 == 5);
}

TEST_CASE("FortranEvaluator interface body to a function of the same cell as actual argument") {
    // {implicit_interface, implicit_typing}
    for (auto mode : {std::make_pair(false, false), std::make_pair(true, false),
            std::make_pair(true, true)}) {
        CAPTURE(mode.first);
        CAPTURE(mode.second);
        CompilerOptions cu;
        cu.interactive = true;
        cu.implicit_interface = mode.first;
        cu.implicit_typing = mode.second;
        cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
        FortranEvaluator e(cu);
        REQUIRE(e.evaluate2("integer :: z0").ok);
        REQUIRE(e.evaluate2(R"(integer function app(f)
interface
integer function f()
end function
end interface
app = f() + 1
end function
)").ok);
        // `gg` is declared by an interface body, so passing it is not a
        // reference to a variable.
        REQUIRE(e.evaluate2(R"(integer function gg()
gg = 8
end function

integer function c_iface()
interface
integer function gg()
end function
end interface
c_iface = app(gg) + 300
end function
)").ok);
        LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2("c_iface()");
        REQUIRE(r.ok);
        CHECK(r.result.i32 == 309);
        // The same in a procedure with ENTRY, whose statements are visited
        // in the scope of its master function.
        REQUIRE(e.evaluate2(R"(integer function gg2()
gg2 = 8
end function

integer function c_entry_iface()
interface
integer function gg2()
end function
end interface
c_entry_iface = app(gg2) + 400
return
entry c_entry_iface2()
c_entry_iface2 = app(gg2) + 410
end function
)").ok);
        r = e.evaluate2("c_entry_iface()");
        REQUIRE(r.ok);
        CHECK(r.result.i32 == 409);
        r = e.evaluate2("c_entry_iface2()");
        REQUIRE(r.ok);
        CHECK(r.result.i32 == 419);
        if (!mode.first) continue;
        // The same with `integer, external`.
        REQUIRE(e.evaluate2(R"(integer function hh()
hh = 9
end function

integer function c_ext()
integer, external :: hh
c_ext = app(hh) + 310
end function
)").ok);
        r = e.evaluate2("c_ext()");
        REQUIRE(r.ok);
        CHECK(r.result.i32 == 320);
        // The same with ENTRY.
        REQUIRE(e.evaluate2(R"(integer function hh2()
hh2 = 9
end function

integer function c_entry_ext()
integer, external :: hh2
c_entry_ext = app(hh2) + 420
return
entry c_entry_ext2()
c_entry_ext2 = app(hh2) + 430
end function
)").ok);
        r = e.evaluate2("c_entry_ext()");
        REQUIRE(r.ok);
        CHECK(r.result.i32 == 430);
        r = e.evaluate2("c_entry_ext2()");
        REQUIRE(r.ok);
        CHECK(r.result.i32 == 440);
    }
}

TEST_CASE("FortranEvaluator dummy procedure of an ENTRY statement named like an earlier cell's procedure") {
    // {implicit_interface, implicit_typing}
    for (auto mode : {std::make_pair(false, false), std::make_pair(true, false),
            std::make_pair(true, true)}) {
        CAPTURE(mode.first);
        CAPTURE(mode.second);
        CompilerOptions cu;
        cu.interactive = true;
        cu.implicit_interface = mode.first;
        cu.implicit_typing = mode.second;
        cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
        FortranEvaluator e(cu);
        REQUIRE(e.evaluate2("integer :: z0").ok);
        REQUIRE(e.evaluate2(R"(integer function fn()
fn = 1
end function

subroutine sn(x)
integer, intent(out) :: x
x = 4
end subroutine

integer function ifn()
ifn = 5
end function
)").ok);
        // `fn` is a dummy argument of the ENTRY statement only, so its
        // interface body declares that dummy, not the earlier cell's `fn`.
        REQUIRE(e.evaluate2(R"(integer function c_iface()
interface
integer function fn()
end function
end interface
integer :: c_iface2
c_iface = 10
return
entry c_iface2(fn)
c_iface2 = fn() + 30
end function
)").ok);
        LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2("c_iface()");
        REQUIRE(r.ok);
        CHECK(r.result.i32 == 10);
        r = e.evaluate2("c_iface2(ifn)");
        REQUIRE(r.ok);
        CHECK(r.result.i32 == 35);
        if (!mode.first) continue;
        // The same with `integer, external`.
        REQUIRE(e.evaluate2(R"(integer function c_ext()
integer, external :: fn
integer :: c_ext2
c_ext = 10
return
entry c_ext2(fn)
c_ext2 = fn() + 20
end function
)").ok);
        r = e.evaluate2("c_ext()");
        REQUIRE(r.ok);
        CHECK(r.result.i32 == 10);
        r = e.evaluate2("c_ext2(ifn)");
        REQUIRE(r.ok);
        CHECK(r.result.i32 == 25);
        // The same with an EXTERNAL statement in a subroutine.
        REQUIRE(e.evaluate2(R"(subroutine s(r)
integer, intent(out) :: r
external sn
r = 10
return
entry s2(r, sn)
call sn(r)
r = r + 40
end subroutine
)").ok);
        REQUIRE(e.evaluate2("integer :: q").ok);
        REQUIRE(e.evaluate2("call s2(q, sn)").ok);
        r = e.evaluate2("q");
        REQUIRE(r.ok);
        CHECK(r.result.i32 == 44);
    }
}

TEST_CASE("FortranEvaluator external statement for an earlier cell's function") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.implicit_interface = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    REQUIRE(e.evaluate2("integer :: z0").ok);
    REQUIRE(e.evaluate2(R"(integer function fn()
fn = 7
end function
)").ok);
    REQUIRE(e.evaluate2(R"(integer function call_fn()
    integer, external :: fn
    call_fn = fn()
end function
)").ok);
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2("call_fn()");
    REQUIRE(r.ok);
    CHECK(r.result.i32 == 7);
}

TEST_CASE("FortranEvaluator interface body used by a contained procedure") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    REQUIRE(e.evaluate2("integer :: z0").ok);
    REQUIRE(e.evaluate2(R"(integer function fn()
fn = 1
end function
)").ok);
    // The contained procedure declares a procedure pointer with the host's
    // interface, which is the earlier cell's `fn`.
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2(R"(integer function c()
interface
integer function fn()
end function
end interface
c = g()
contains
integer function g()
procedure(fn), pointer :: p => null()
g = 6
if (associated(p)) g = 0
end function
end function
)");
    REQUIRE(r.ok);
    r = e.evaluate2("c()");
    REQUIRE(r.ok);
    CHECK(r.result.i32 == 6);
}

TEST_CASE("FortranEvaluator interface body in a module used by another module") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    REQUIRE(e.evaluate2("integer :: z0").ok);
    REQUIRE(e.evaluate2(R"(integer function fn()
fn = 1
end function
)").ok);
    // A module's interface body is an entity of the module, which `use`
    // makes accessible, so it stays.
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2(R"(module m_iface
interface
integer function fn()
end function
end interface
end module
module m2_iface
use m_iface
contains
integer function h()
h = 7
end function
end module
)");
    REQUIRE(r.ok);
    REQUIRE(e.evaluate2("use m2_iface").ok);
    r = e.evaluate2("h()");
    REQUIRE(r.ok);
    CHECK(r.result.i32 == 7);
}

#if !defined(__EMSCRIPTEN__) && LLVM_VERSION_MAJOR != 8
// The evaluator's message for a symbol the JIT cannot resolve; LLVM 7
// reports it from findSymbol()/getAddress(), newer LLVM from lookup().
// In the WASM build an unresolved symbol is instead a JavaScript "Dynamic
// linking error" that aborts the process, so the checks below that call a
// cell that does not link are native only. They are also skipped with
// LLVM 8, whose ORC ExecutionSession::lookup() reports "Symbols not found"
// but then never returns when a symbol fails to materialize.
static bool is_jit_symbol_lookup_failure(const std::string &msg) {
    return msg.find("lookup() failed to find the symbol") != std::string::npos
        || msg.find("findSymbol() failed to find the symbol") != std::string::npos
        || msg.find("getAddress() failed for symbol") != std::string::npos;
}
#endif

TEST_CASE("FortranEvaluator interface body in a generic interface block") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    REQUIRE(e.evaluate2("integer :: z0").ok);
    REQUIRE(e.evaluate2(R"(integer function fn()
fn = 1
end function
)").ok);
    // The generic interface refers to the interface body, so it stays and
    // the cell compiles; as without an earlier `fn`, a call of `c` then
    // does not link.
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2(R"(integer function c()
interface gen
integer function fn()
end function
end interface
c = gen()
end function
)");
    CHECK(r.ok);
#if !defined(__EMSCRIPTEN__) && LLVM_VERSION_MAJOR != 8
    // The unresolved `fn` makes the JIT lookup of the cell fail.
    bool linked = false;
    try {
        r = e.evaluate2("c()");
        linked = r.ok;
    } catch (const LCompilers::LCompilersException &ex) {
        std::string msg = ex.msg();
        CAPTURE(msg);
        CHECK(is_jit_symbol_lookup_failure(msg));
    }
    CHECK(!linked);
#endif
}

TEST_CASE("FortranEvaluator abstract interface named as an earlier cell's function") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    REQUIRE(e.evaluate2("integer :: z0").ok);
    REQUIRE(e.evaluate2(R"(integer function fn()
fn = 1
end function
)").ok);
    // An abstract interface declares no procedure, so it does not refer to
    // the earlier cell's `fn` and cannot be referenced.
    LCompilers::LocationManager lm;
    LCompilers::PassManager lpm;
    lpm.use_default_passes();
    LCompilers::diag::Diagnostics d;
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate(R"(integer function c()
abstract interface
integer function fn()
end function
end interface
c = fn()
end function
)", false, lm, lpm, d);
    CHECK(!r.ok);
    cu.use_colors = false;
    CHECK(d.render(lm, cu).find("ASR verify") == std::string::npos);
}

TEST_CASE("FortranEvaluator external statement of another type than an earlier cell's function") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.implicit_interface = true;
    cu.implicit_typing = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    REQUIRE(e.evaluate2("integer :: z0").ok);
    REQUIRE(e.evaluate2(R"(integer function fn()
fn = 3
end function
)").ok);
    REQUIRE(e.evaluate2(R"(real function gn()
gn = 3
end function
)").ok);
    REQUIRE(e.evaluate2(R"(subroutine sn(x)
integer, intent(out) :: x
x = 4
end subroutine
)").ok);
    // `gn` has the implicit type real, like the earlier cell's `gn`, and
    // `sn` is a subroutine.
    REQUIRE(e.evaluate2(R"(real function c_gn()
external gn, sn
integer :: k
call sn(k)
c_gn = gn() / 2 + k
end function
)").ok);
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2("c_gn()");
    REQUIRE(r.ok);
    CHECK(r.result.f32 == 5.5f);
    // A type declaration statement after EXTERNAL gives the type.
    REQUIRE(e.evaluate2(R"(real function c_typed()
external fn
integer fn
c_typed = fn() / 2
end function
)").ok);
    r = e.evaluate2("c_typed()");
    REQUIRE(r.ok);
    CHECK(r.result.f32 == 1.0f);
#ifndef __EMSCRIPTEN__
    // In the WASM build the side module of this cell, which calls an
    // undefined `fn`, does not load ("function index ... is out of bounds",
    // like #17138).
    // `fn` has the implicit type real, so the external procedure it
    // declares is not the earlier cell's integer `fn`: as without that
    // `fn`, a call of `c` does not link.
    REQUIRE(e.evaluate2(R"(real function c()
external fn
c = fn() / 2
end function
)").ok);
#if LLVM_VERSION_MAJOR != 8
    // The unresolved `fn` makes the JIT lookup of the cell fail.
    bool linked = false;
    try {
        r = e.evaluate2("c()");
        linked = r.ok;
    } catch (const LCompilers::LCompilersException &ex) {
        std::string msg = ex.msg();
        CAPTURE(msg);
        CHECK(is_jit_symbol_lookup_failure(msg));
    }
    CHECK(!linked);
#endif
#endif
}

TEST_CASE("FortranEvaluator interface body of an earlier cell's procedure in scoping units") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    REQUIRE(e.evaluate2("integer :: z0").ok);
    REQUIRE(e.evaluate2(R"(integer function fn()
fn = 1
end function
)").ok);
    REQUIRE(e.evaluate2(R"(subroutine s(x)
integer, intent(out) :: x
x = 9
end subroutine
)").ok);
#ifndef __EMSCRIPTEN__
    // Procedure pointers do not work in the WASM interactive executor yet
    // (#17138): the side module of a cell that takes a procedure's address is
    // rejected with "function index ... is out of bounds".
    // A procedure pointer declared with the interface, a subroutine
    // referenced by CALL, a dummy procedure named like an earlier cell's
    // procedure, and a contained procedure that uses its host's interface.
    REQUIRE(e.evaluate2(R"(integer function c()
interface
integer function fn()
end function
subroutine s(x)
integer, intent(out) :: x
end subroutine
end interface
procedure(fn), pointer :: p
integer :: k
p => fn
call s(k)
c = p() + k * 10 + g()
contains
integer function g()
procedure(fn), pointer :: q
q => fn
g = q() * 100
end function
end function
)").ok);
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2("c()");
    REQUIRE(r.ok);
    CHECK(r.result.i32 == 191);
    REQUIRE(e.evaluate2(R"(integer function app(fn)
interface
integer function fn()
end function
end interface
app = fn() + 100
end function
)").ok);
    REQUIRE(e.evaluate2(R"(integer function d()
d = 7
end function
)").ok);
    r = e.evaluate2("app(d)");
    REQUIRE(r.ok);
    CHECK(r.result.i32 == 107);
#endif
    // A module procedure and a BLOCK construct.
    REQUIRE(e.evaluate2(R"(module m_cell_iface
contains
integer function h()
interface
integer function fn()
end function
end interface
h = fn() + 50
block
interface
subroutine s(x)
integer, intent(out) :: x
end subroutine
end interface
integer :: k
call s(k)
h = h + k
end block
end function
end module
)").ok);
    REQUIRE(e.evaluate2("use m_cell_iface").ok);
    LCompilers::Result<FortranEvaluator::EvalResult> rh = e.evaluate2("h()");
    REQUIRE(rh.ok);
    CHECK(rh.result.i32 == 60);
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

TEST_CASE("FortranEvaluator generic subroutine called from a cell") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    // Issue 16710: a cell that calls a module's generic subroutine, after
    // another cell used the module, crashed the kernel.
    CHECK(e.evaluate2(R"(module mgsub
implicit none
integer :: last = 0
interface show
    module procedure show_int, show_real
end interface
contains
    subroutine show_int(i)
        integer, intent(in) :: i
        last = i
    end subroutine
    subroutine show_real(x)
        real(8), intent(in) :: x
        last = -int(x)
    end subroutine
end module
)").ok);
    CHECK(e.evaluate2("use mgsub").ok);
    CHECK(e.evaluate2("call show(3)").ok);
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2("last");
    REQUIRE(r.ok);
    CHECK(r.result.i32 == 3);
    CHECK(e.evaluate2("call show(5.0d0)").ok);
    r = e.evaluate2("last");
    REQUIRE(r.ok);
    CHECK(r.result.i32 == -5);

    // The use and the call in one cell.
    FortranEvaluator e2(cu);
    CHECK(e2.evaluate2(R"(module mgsub2
implicit none
integer :: last = 0
interface show
    module procedure show_int
end interface
contains
    subroutine show_int(i)
        integer, intent(in) :: i
        last = i
    end subroutine
end module
)").ok);
    CHECK(e2.evaluate2(R"(use mgsub2
call show(7)
)").ok);
    r = e2.evaluate2("last");
    REQUIRE(r.ok);
    CHECK(r.result.i32 == 7);

    // Specifics with optional arguments, and a generic function.
    FortranEvaluator e3(cu);
    CHECK(e3.evaluate2(R"(module mgsub3
implicit none
integer :: last = 0
interface put
    module procedure put_int, put_real
end interface
interface twice
    module procedure twice_int, twice_real
end interface
contains
    subroutine put_int(i, scale)
        integer, intent(in) :: i
        integer, intent(in), optional :: scale
        last = i
        if (present(scale)) last = last * scale
    end subroutine
    subroutine put_real(x, scale)
        real(8), intent(in) :: x
        integer, intent(in), optional :: scale
        last = int(x)
        if (present(scale)) last = last * scale
    end subroutine
    integer function twice_int(i)
        integer, intent(in) :: i
        twice_int = 2 * i
    end function
    real(8) function twice_real(x)
        real(8), intent(in) :: x
        twice_real = 2 * x
    end function
end module
)").ok);
    CHECK(e3.evaluate2("use mgsub3").ok);
    CHECK(e3.evaluate2("call put(4, scale=3)").ok);
    r = e3.evaluate2("last");
    REQUIRE(r.ok);
    CHECK(r.result.i32 == 12);
    CHECK(e3.evaluate2("call put(2.0d0)").ok);
    r = e3.evaluate2("last");
    REQUIRE(r.ok);
    CHECK(r.result.i32 == 2);
    r = e3.evaluate2("twice(21)");
    REQUIRE(r.ok);
    CHECK(r.result.i32 == 42);
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

// An array of a derived type that a cell declares with a constant array
// initializer keeps the initializer's element values rather than only the
// components' defaults (#16945).
TEST_CASE("FortranEvaluator derived type array constant initializer") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);

    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("type :: arr_init_t\ninteger :: k\nend type");
    CHECK(r.ok);
    r = e.evaluate2("type(arr_init_t) :: arr_init_a(2) = [arr_init_t(5), arr_init_t(6)]");
    CHECK(r.ok);
    r = e.evaluate2("arr_init_a(1)%k");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 5);
    r = e.evaluate2("arr_init_a(2)%k");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 6);

    // A component default, overridden by the initializer.
    r = e.evaluate2("type :: arr_init_d\ninteger :: k = 3, m = 4\nend type");
    CHECK(r.ok);
    r = e.evaluate2("type(arr_init_d) :: arr_init_b(3) = [arr_init_d(7, 8), arr_init_d(9, 10), arr_init_d(11, 12)]");
    CHECK(r.ok);
    r = e.evaluate2("arr_init_b(3)%m");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 12);
    r = e.evaluate2("arr_init_b(2)%k");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 9);

    // A scalar structure constructor broadcast to every element.
    r = e.evaluate2("type(arr_init_t) :: arr_init_s(3) = arr_init_t(15)");
    CHECK(r.ok);
    r = e.evaluate2("arr_init_s(1)%k + arr_init_s(3)%k");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 30);

    // The array stays assignable after its static initialization.
    r = e.evaluate2("arr_init_a(1)%k = 20");
    CHECK(r.ok);
    r = e.evaluate2("arr_init_a(1)%k + arr_init_a(2)%k");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 26);

    // The type, the declaration and a use of it in a single cell.
    r = e.evaluate2(R"(type :: arr_init_c
integer :: k
end type
type(arr_init_c) :: arr_init_v(2) = [arr_init_c(13), arr_init_c(14)]
arr_init_v(1)%k + arr_init_v(2)%k)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 27);

    // A type with an allocatable component: the static data and the
    // component's runtime setup compose, and it can be allocated later.
    r = e.evaluate2("type :: arr_init_u\ninteger, allocatable :: x(:)\ninteger :: k = 1\nend type");
    CHECK(r.ok);
    r = e.evaluate2("type(arr_init_u) :: arr_init_w(2) = [arr_init_u(k=5), arr_init_u(k=6)]");
    CHECK(r.ok);
    r = e.evaluate2("10 * arr_init_w(1)%k + arr_init_w(2)%k + merge(100, 0, allocated(arr_init_w(1)%x))");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 56);
    r = e.evaluate2("allocate(arr_init_w(1)%x(3))");
    CHECK(r.ok);
    r = e.evaluate2("arr_init_w(1)%x = 7");
    CHECK(r.ok);
    r = e.evaluate2("sum(arr_init_w(1)%x) + arr_init_w(2)%k");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 27);

    // A type with array and character components.
    r = e.evaluate2("type :: arr_init_e\ninteger :: a(2) = [1, 2]\ncharacter(len=3) :: s = \"abc\"\nend type");
    CHECK(r.ok);
    r = e.evaluate2("type(arr_init_e) :: arr_init_f(2) = [arr_init_e([3, 4], \"xyz\"), arr_init_e()]");
    CHECK(r.ok);
    r = e.evaluate2("1000 * arr_init_f(1)%a(1) + 100 * arr_init_f(1)%a(2) + 10 * arr_init_f(2)%a(1) + arr_init_f(2)%a(2)");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 3412);
    r = e.evaluate2("merge(1, 0, arr_init_f(1)%s == \"xyz\") + merge(10, 0, arr_init_f(2)%s == \"abc\")");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 11);
}

// An array of a derived type that a cell declares with an implied-do array
// constructor of structure constructors is initialized statically (#13226).
TEST_CASE("FortranEvaluator derived type array implied do initializer") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);

    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("type :: idl_init_t\ninteger :: k = 3\nend type");
    CHECK(r.ok);
    r = e.evaluate2("integer :: idl_i");
    CHECK(r.ok);
    r = e.evaluate2("type(idl_init_t) :: idl_init_a(3) = [(idl_init_t(idl_i*10), idl_i=1,3)]");
    CHECK(r.ok);
    r = e.evaluate2("100 * idl_init_a(1)%k + 10 * idl_init_a(2)%k / 10 + idl_init_a(3)%k");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 1050);

    // The declaration and a use of the array in a single cell.
    r = e.evaluate2("type(idl_init_t) :: idl_init_b(2) = [(idl_init_t(idl_i + 1), idl_i=1,2)]\nidl_init_b(1)%k * idl_init_b(2)%k");
    CHECK(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 6);
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

TEST_CASE("FortranEvaluator execute: formatted print of an array section") {
    // The items of an array section share one format, as in a compiled
    // program (#16712).
    CompilerOptions cu = execute_options();
    FortranEvaluator e(cu);
    std::string output;
    FortranEvaluator::CellIO io;
    io.output = [&](int, const std::string &text) { output += text; };
    auto check_output = [&](const std::string &code,
            const std::string &expected) {
        output.clear();
        auto cell = e.execute(code, false, io);
        CAPTURE(code);
        REQUIRE_MESSAGE(cell.ok, cell.error_text());
        CHECK(output == expected);
    };
    REQUIRE(e.execute("real(8) :: a(3, 0:4)").ok);
    REQUIRE(e.execute("a = 1").ok);
    check_output("print \"(3f9.3)\", a(:, 4)",
        "    1.000    1.000    1.000\n");
    check_output("print \"(a, 3f9.3)\", \"a:\", a(:, 4)",
        "a:    1.000    1.000    1.000\n");
    REQUIRE(e.execute("integer, allocatable :: v(:)").ok);
    REQUIRE(e.execute("allocate(v(4))").ok);
    REQUIRE(e.execute("v = [1, 2, 3, 4]").ok);
    check_output("print \"(a, 4i2, a)\", \"v:\", v, \".\"",
        "v: 1 2 3 4.\n");
    check_output("print \"(a, 2i2)\", \"v:\", v(2:3)", "v: 2 3\n");
    REQUIRE(e.execute("type :: point\n    integer :: x, y\nend type").ok);
    REQUIRE(e.execute("type(point) :: p").ok);
    REQUIRE(e.execute("p = point(1, 2)").ok);
    check_output("print \"(a, 2i2)\", \"p:\", p", "p: 1 2\n");
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
TEST_CASE("FortranEvaluator execute: streaming hook affinity and failure cleanup") {
    CompilerOptions cu = execute_options();
    FortranEvaluator e(cu);
    auto owner = std::this_thread::get_id();
    std::string output, events;
    FortranEvaluator::CellIO io;
    io.output = [&](int fd, const std::string &text) {
        CHECK(std::this_thread::get_id() == owner);
        CHECK(fd == 1);
        output += text;
    };
    io.display = [&](const std::string &, const std::string &) {
        CHECK(std::this_thread::get_id() == owner);
        CHECK(output == "printed\n");
        events += "display ";
    };
    io.clear_output = [&]() {
        CHECK(std::this_thread::get_id() == owner);
        CHECK(output == "printed\n");
        events += "clear ";
    };
    io.input = [&](std::string &record) {
        CHECK(std::this_thread::get_id() == owner);
        CHECK(output == "printed\nprompt");
        events += "input";
        record = "42";
        return 0;
    };
    auto cell = e.execute(R"(
use lfortran_display
integer :: answer, ios
print '(a)', 'printed'
call display_data('text/plain', 'shown')
call clear_output()
write(*,'(a)',advance='no') 'prompt'
read(*,*,iostat=ios) answer
if (ios /= 0 .or. answer /= 42) error stop
)", false, io);
    REQUIRE_MESSAGE(cell.ok, cell.error_text());
    CHECK(events == "display clear input");
    io.clear_output = [&]() {
        CHECK(std::this_thread::get_id() == owner);
        throw std::runtime_error("clear failed");
    };
    cell = e.execute("call clear_output()", false, io);
    CHECK(!cell.ok);
    CHECK(cell.error_name == "DisplayError");
    CHECK(cell.error_value == "display failed: clear failed");
    io.input = [&](std::string &) -> int {
        CHECK(std::this_thread::get_id() == owner);
        throw std::runtime_error("input failed");
    };
    cell = e.execute("read(*,*,iostat=ios) answer\nif (ios <= 0) error stop", false, io);
    REQUIRE_MESSAGE(cell.ok, cell.error_text());
    io.output = [&](int, const std::string &) {
        CHECK(std::this_thread::get_id() == owner);
        throw std::runtime_error("publication failed");
    };
    cell = e.execute("print *, 'failure'\ncall clear_output()", false, io);
    CHECK(!cell.ok);
    CHECK(cell.error_name == "ExecutionError");
    CHECK(cell.error_value == "publication failed");
    io.output = [&](int, const std::string &text) { output += text; };
    output.clear();
    cell = e.execute("print '(a)', 'recovered'", false, io);
    REQUIRE_MESSAGE(cell.ok, cell.error_text());
    CHECK(output == "recovered\n");
    io.progress_output = [&]() {
        CHECK(std::this_thread::get_id() == owner);
        throw std::runtime_error("transport failed");
    };
    cell = e.execute("print '(a)', 'transport failure'", false, io);
    CHECK(!cell.ok);
    CHECK(cell.error_name == "ExecutionError");
    CHECK(cell.error_value == "transport failed");
    io.progress_output = {};
    output.clear();
    cell = e.execute("print '(a)', 'transport recovered'", false, io);
    REQUIRE_MESSAGE(cell.ok, cell.error_text());
    CHECK(output == "transport recovered\n");
}

TEST_CASE("Kernel streams: split UTF-8 and invalid bytes") {
    using namespace LCompilers::LFortran::kernel;
    std::string streams[2];
    std::function<void(const std::string &, const std::string &)> publish =
        [&](const std::string &name, const std::string &text) {
            CHECK(LCompilers::is_valid_utf8(text));
            streams[name == "stdout" ? 0 : 1] += text;
        };
    StreamText text(publish);
    text.write(1, "partial\xe2");
    CHECK(streams[0] == "partial");
    text.write(2, "\xf0\x9f");
    CHECK(streams[1].empty());
    text.write(1, "\x82");
    CHECK(streams[0] == "partial");
    text.write(1, "\xac");
    text.write(2, "\x98\x80");
    CHECK(streams[0] == "partial\xe2\x82\xac");
    CHECK(streams[1] == "\xf0\x9f\x98\x80");
    text.write(1, "\xff\xe2\x82");
    text.finish();
    CHECK(streams[0] == "partial\xe2\x82\xac" + utf8("\xff\xe2\x82"));
    text.write(1, "\xe0\x80");
    text.write(1, "\x80");
    CHECK(streams[0] == "partial\xe2\x82\xac" +
        utf8("\xff\xe2\x82") + utf8("\xe0\x80\x80"));
}

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

#ifndef _WIN32
// The cell runs on a thread of its own while the caller passes its output on.
// That thread must have the stack the cell had on the main thread, not the
// default for a new thread, which on macOS is 512 KiB (#16348). glibc's
// default follows RLIMIT_STACK, so it is lowered here to what macOS has.
TEST_CASE("FortranEvaluator execute: a streaming cell keeps the main thread's stack") {
    struct rlimit limit;
    REQUIRE(getrlimit(RLIMIT_STACK, &limit) == 0);
    if (limit.rlim_cur != RLIM_INFINITY && limit.rlim_cur < 8 * 1024 * 1024) {
        MESSAGE("RLIMIT_STACK is below 8 MiB; skipping");
        return;
    }
// pthread_getattr_default_np and pthread_setattr_default_np were added in
// glibc 2.18. The conda-forge linux build compiles against the glibc 2.17
// sysroot, whose pthread.h does not declare them. Keep the __GLIBC__ test
// outside the version check: Clang rejects an undefined function-like macro
// in #if, so macOS must not expand __GLIBC_PREREQ.
#ifdef __GLIBC__
#if __GLIBC_PREREQ(2, 18)
    struct DefaultStack {
        pthread_attr_t saved;
        DefaultStack() {
            REQUIRE(pthread_getattr_default_np(&saved) == 0);
            pthread_attr_t small;
            REQUIRE(pthread_attr_init(&small) == 0);
            REQUIRE(pthread_attr_setstacksize(&small, 512 * 1024) == 0);
            REQUIRE(pthread_setattr_default_np(&small) == 0);
            pthread_attr_destroy(&small);
        }
        ~DefaultStack() {
            pthread_setattr_default_np(&saved);
            pthread_attr_destroy(&saved);
        }
    } default_stack;
#endif
#endif
    CompilerOptions cu = execute_options();
    FortranEvaluator e(cu);
    std::string output;
    FortranEvaluator::CellIO io;
    io.output = [&](int, const std::string &text) { output += text; };
    run_cell(e, R"(
recursive function depth(n) result(r)
integer, intent(in) :: n
integer :: r
if (n == 0) then
    r = 0
else
    r = 1 + depth(n - 1)
end if
end function
)", io);
    // 2 to 4 MiB of frames: well beyond 512 KiB, well within 8 MiB.
    run_cell(e, "print '(i0)', depth(100000)", io);
    CHECK(output == "100000\n");
}
#endif

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

TEST_CASE("FortranEvaluator allocate in an interactive cell") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    // The notebook cell from issue 13455. A bare allocate is the statement,
    // so the cell runs and the sum is 6. deallocate is the same parse.
    const char *cell = R"(integer, allocatable :: a(:)
allocate(a(3))
a = [1, 2, 3]
print *, sum(a)
)";
    REQUIRE(e.evaluate2(cell).ok);
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2("sum(a)");
    REQUIRE(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 6);
    REQUIRE(e.evaluate2("deallocate(a)").ok);
    r = e.evaluate2("allocated(a)");
    REQUIRE(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::boolean);
    CHECK(!r.result.b);

    // A call in an expression is still a user function named allocate.
    // A bare allocate() stays the statement, as it does in a program.
    FortranEvaluator e2(cu);
    REQUIRE(e2.evaluate2(R"(integer function allocate(n)
integer, intent(in) :: n
allocate = n + 1
end function
integer :: y
y = allocate(2)
y
)").ok);
    r = e2.evaluate2("y");
    REQUIRE(r.ok);
    CHECK(r.result.i32 == 3);
    REQUIRE(e2.evaluate2(R"(integer, allocatable :: b(:)
allocate(b(1))
b(1) = 4
)").ok);
    r = e2.evaluate2("sum(b)");
    REQUIRE(r.ok);
    CHECK(r.result.i32 == 4);
    REQUIRE(e2.evaluate2(R"(subroutine deallocate(n)
integer, intent(inout) :: n
n = n + 7
end subroutine
integer :: n
n = 1
call deallocate(n)
)").ok);
    r = e2.evaluate2("n");
    REQUIRE(r.ok);
    CHECK(r.result.i32 == 8);
}

TEST_CASE("FortranEvaluator allocate source mold and type parameter") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    // source=, mold=, a function source, and a shared type parameter each
    // emit a prologue. An interactive cell has to run that prologue.
    {
        FortranEvaluator e(cu);
        REQUIRE(e.evaluate2(R"(integer, allocatable :: c(:)
allocate(c, source=[1, 2])
)").ok);
        LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2("sum(c)");
        REQUIRE(r.ok);
        CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
        CHECK(r.result.i32 == 3);
    }
    {
        FortranEvaluator e(cu);
        REQUIRE(e.evaluate2(R"(integer :: a(2)
a = [4, 5]
integer, allocatable :: d(:)
allocate(d, mold=a)
)").ok);
        LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2("size(d)");
        REQUIRE(r.ok);
        CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
        CHECK(r.result.i32 == 2);
        r = e.evaluate2("allocated(d)");
        REQUIRE(r.ok);
        CHECK(r.result.type == FortranEvaluator::EvalResult::boolean);
        CHECK(r.result.b);
    }
    {
        FortranEvaluator e(cu);
        REQUIRE(e.evaluate2(R"(integer function alloc_source_fn_13455()
alloc_source_fn_13455 = 7
end function
integer, allocatable :: f
allocate(f, source=alloc_source_fn_13455())
)").ok);
        // An allocatable scalar is not a supported cell result, so read it
        // through an integer expression.
        LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2("f + 0");
        REQUIRE(r.ok);
        CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
        CHECK(r.result.i32 == 7);
    }
    {
        FortranEvaluator e(cu);
        REQUIRE(e.evaluate2(R"(integer :: n
n = 1
character(:), allocatable :: s, t
allocate(character(n+1) :: s, t)
)").ok);
        LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2("len(s)");
        REQUIRE(r.ok);
        CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
        CHECK(r.result.i32 == 2);
        r = e.evaluate2("len(t)");
        REQUIRE(r.ok);
        CHECK(r.result.i32 == 2);
    }
}

TEST_CASE("FortranEvaluator assumed-shape procedure called from a later cell") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    // A procedure with an assumed-shape intent dummy also gets an
    // array-by-data copy. A later cell can still call the original with
    // a descriptor, so the cell that defines it has to keep it.
    {
        FortranEvaluator e(cu);
        REQUIRE(e.evaluate2(R"(subroutine fill_16711(x)
real(8), intent(out) :: x(:)
x = 1.0d0
end subroutine
)").ok);
        REQUIRE(e.evaluate2(R"(real(8) :: v(4)
v = 0.0d0
call fill_16711(v)
)").ok);
        LCompilers::Result<FortranEvaluator::EvalResult> r
            = e.evaluate2("sum(v)");
        REQUIRE(r.ok);
        CHECK(r.result.type == FortranEvaluator::EvalResult::real8);
        CHECK(r.result.f64 == 4.0);
    }
    {
        FortranEvaluator e(cu);
        REQUIRE(e.evaluate2(R"(real(8) function total_16711(x)
real(8), intent(in) :: x(:)
total_16711 = sum(x)
end function
)").ok);
        LCompilers::Result<FortranEvaluator::EvalResult> r
            = e.evaluate2("total_16711([1.0d0, 2.0d0, 3.0d0])");
        REQUIRE(r.ok);
        CHECK(r.result.type == FortranEvaluator::EvalResult::real8);
        CHECK(r.result.f64 == 6.0);
    }
    {
        FortranEvaluator e(cu);
        REQUIRE(e.evaluate2(R"(module m_16711
implicit none
contains
real(8) function row_sum_16711(x)
real(8), intent(in) :: x(:)
row_sum_16711 = sum(x)
end function
end module
)").ok);
        REQUIRE(e.evaluate2(R"(use m_16711
real(8) :: a(2, 3)
a = reshape([1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0, 6.0d0], [2, 3])
)").ok);
        LCompilers::Result<FortranEvaluator::EvalResult> r
            = e.evaluate2("row_sum_16711(a(2, :))");
        REQUIRE(r.ok);
        CHECK(r.result.type == FortranEvaluator::EvalResult::real8);
        CHECK(r.result.f64 == 12.0);
    }
}

#ifndef __EMSCRIPTEN__
// In the WASM test build, destroying an LLVMEvaluator traps (memory access
// out of bounds); the other LLVMEvaluator tests are excluded from it too.
namespace {

// sum(1/sqrt(i)**3, i = 1, n) in single precision, with fast math.
const std::string rsqrt_sum_ir = R"""(
define float @rsqrt_sum(i32 %n) {
entry:
  br label %loop

loop:
  %i = phi i32 [ 1, %entry ], [ %i.next, %loop ]
  %acc = phi float [ 0.0, %entry ], [ %acc.next, %loop ]
  %x = sitofp i32 %i to float
  %s = call fast float @llvm.sqrt.f32(float %x)
  %r = fdiv fast float 1.0, %s
  %r2 = fmul fast float %r, %r
  %r3 = fmul fast float %r2, %r
  %acc.next = fadd fast float %acc, %r3
  %i.next = add i32 %i, 1
  %done = icmp sgt i32 %i.next, %n
  br i1 %done, label %exit, label %loop

exit:
  ret float %acc.next
}

declare float @llvm.sqrt.f32(float)
)""";

llvm::BasicBlock *block_named(llvm::Function &f, const std::string &name) {
    for (llvm::BasicBlock &bb: f) {
        if (bb.getName() == name) return &bb;
    }
    return nullptr;
}

// The constant factors that the block multiplies values by.
std::vector<double> constant_factors(llvm::BasicBlock &bb) {
    std::vector<double> factors;
    for (llvm::Instruction &i: bb) {
        if (i.getOpcode() != llvm::Instruction::FMul) continue;
        for (llvm::Value *op: i.operands()) {
            llvm::Constant *c = llvm::dyn_cast<llvm::Constant>(op);
            if (!c) continue;
            if (c->getType()->isVectorTy()) c = c->getSplatValue();
            if (llvm::ConstantFP *fp = llvm::dyn_cast_or_null<llvm::ConstantFP>(c)) {
                factors.push_back(fp->getType()->isFloatTy()
                    ? fp->getValueAPF().convertToFloat()
                    : fp->getValueAPF().convertToDouble());
            }
        }
    }
    return factors;
}

} // namespace

TEST_CASE("LLVM reciprocal square roots with Newton steps in IR") {
    // x86: one Newton step, 1/sqrt(x) = z / (-2), so the sum of the cubes
    // is multiplied by (-1/2)**3 once, after the loop.
    {
        LCompilers::LLVMEvaluator e;
        std::unique_ptr<llvm::Module> m = e.parse_module(rsqrt_sum_ir, "");
        llvm::Function &f = *m->getFunction("rsqrt_sum");
        CHECK(LCompilers::expand_reciprocal_sqrt(f,
            llvm::Triple("x86_64-unknown-linux-gnu")));
        CHECK(f.getFnAttribute("reciprocal-estimates").getValueAsString()
            == "sqrtf:0,vec-sqrtf:0");
        CHECK(constant_factors(*block_named(f, "loop")).empty());
        CHECK(constant_factors(*block_named(f, "exit"))
            == std::vector<double>{-0.125});
        std::string err;
        llvm::raw_string_ostream os(err);
        CHECK(!llvm::verifyFunction(f, &os));
    }
    // Other targets are left to LLVM.
    for (const char *triple: {"aarch64-apple-macosx",
            "riscv64-unknown-linux-gnu"}) {
        LCompilers::LLVMEvaluator e;
        std::unique_ptr<llvm::Module> m = e.parse_module(rsqrt_sum_ir, "");
        llvm::Function &f = *m->getFunction("rsqrt_sum");
        CHECK(!LCompilers::expand_reciprocal_sqrt(f, llvm::Triple(triple)));
        CHECK(!f.hasFnAttribute("reciprocal-estimates"));
    }
    // A square root used other than as a divisor would become a bare
    // estimate too, so the function is left as it is.
    {
        LCompilers::LLVMEvaluator e;
        std::string ir = rsqrt_sum_ir;
        ir.replace(ir.find("  %r2 = fmul fast float %r, %r"),
            std::string("  %r2 = fmul fast float %r, %r").size(),
            "  %r2 = fmul fast float %r, %s");
        std::unique_ptr<llvm::Module> m = e.parse_module(ir, "");
        llvm::Function &f = *m->getFunction("rsqrt_sum");
        CHECK(!LCompilers::expand_reciprocal_sqrt(f,
            llvm::Triple("x86_64-unknown-linux-gnu")));
        CHECK(!f.hasFnAttribute("reciprocal-estimates"));
    }
    // The expanded function computes the sum on this host.
    {
        LCompilers::LLVMEvaluator e;
        std::unique_ptr<llvm::Module> m = e.parse_module(rsqrt_sum_ir, "");
        LCompilers::expand_reciprocal_sqrt(*m->getFunction("rsqrt_sum"),
            e.get_target_machine().getTargetTriple());
        e.add_module(std::move(m));
        double expected = 0;
        for (int i = 1; i <= 1000; i++) expected += std::pow(i, -1.5);
        float sum = e.execfn<float>("rsqrt_sum", 1000);
        CHECK(std::abs(sum - expected) < 1e-5 * expected);
    }
}

namespace {

// sum(1/sqrt(i)**3) in double precision, vectorized 8 lanes, with the
// AVX-512 features the reciprocal square root estimate needs.
const std::string rsqrt_sum_dp_ir = R"""(
define <8 x double> @rsqrt_sum_dp(i32 %n) #0 {
entry:
  br label %loop

loop:
  %i = phi i32 [ 1, %entry ], [ %i.next, %loop ]
  %acc = phi <8 x double> [ zeroinitializer, %entry ], [ %acc.next, %loop ]
  %x = sitofp i32 %i to double
  %v0 = insertelement <8 x double> undef, double %x, i64 0
  %v = shufflevector <8 x double> %v0, <8 x double> undef, <8 x i32> zeroinitializer
  %s = call fast <8 x double> @llvm.sqrt.v8f64(<8 x double> %v)
  %r = fdiv fast <8 x double> <double 1.0, double 1.0, double 1.0, double 1.0, double 1.0, double 1.0, double 1.0, double 1.0>, %s
  %r2 = fmul fast <8 x double> %r, %r
  %r3 = fmul fast <8 x double> %r2, %r
  %acc.next = fadd fast <8 x double> %acc, %r3
  %i.next = add i32 %i, 1
  %done = icmp sgt i32 %i.next, %n
  br i1 %done, label %exit, label %loop

exit:
  ret <8 x double> %acc.next
}

declare <8 x double> @llvm.sqrt.v8f64(<8 x double>)
attributes #0 = { "target-features"="+avx512f,+avx512vl" }
)""";

bool has_factor(llvm::BasicBlock &bb, double factor) {
    for (double f: constant_factors(bb)) {
        if (f == factor) return true;
    }
    return false;
}

bool calls_intrinsic(llvm::Function &f, const std::string &name) {
    for (llvm::BasicBlock &bb: f) {
        for (llvm::Instruction &i: bb) {
            llvm::CallInst *call = llvm::dyn_cast<llvm::CallInst>(&i);
            if (call && call->getCalledFunction()
                    && call->getCalledFunction()->getName() == name) {
                return true;
            }
        }
    }
    return false;
}

} // namespace

TEST_CASE("LLVM double precision reciprocal square roots with Newton steps in IR") {
#ifdef HAVE_TARGET_X86
    // x86 with AVX-512: two Newton steps, 1/sqrt(x) = z/4, so the sum of the
    // cubes is multiplied by (1/4)**3 once, after the loop.
    {
        LCompilers::LLVMEvaluator e;
        std::unique_ptr<llvm::Module> m = e.parse_module(rsqrt_sum_dp_ir, "");
        llvm::Function &f = *m->getFunction("rsqrt_sum_dp");
        CHECK(LCompilers::expand_reciprocal_sqrt(f,
            llvm::Triple("x86_64-unknown-linux-gnu")));
        CHECK(!f.hasFnAttribute("reciprocal-estimates"));
        CHECK(calls_intrinsic(f, "llvm.x86.avx512.rsqrt14.pd.512"));
        CHECK(!has_factor(*block_named(f, "loop"), 0.015625));
        CHECK(has_factor(*block_named(f, "exit"), 0.015625));
        std::string err;
        llvm::raw_string_ostream os(err);
        CHECK(!llvm::verifyFunction(f, &os));
    }
#else
    // Without the x86 target the estimate intrinsic is not available, so the
    // function is left as it is.
    {
        LCompilers::LLVMEvaluator e;
        std::unique_ptr<llvm::Module> m = e.parse_module(rsqrt_sum_dp_ir, "");
        llvm::Function &f = *m->getFunction("rsqrt_sum_dp");
        CHECK(!LCompilers::expand_reciprocal_sqrt(f,
            llvm::Triple("x86_64-unknown-linux-gnu")));
        CHECK(!calls_intrinsic(f, "llvm.x86.avx512.rsqrt14.pd.512"));
        CHECK(!has_factor(*block_named(f, "exit"), 0.015625));
    }
#endif
    // Without the AVX-512 features the estimate is not available, so the
    // function is left as it is.
    {
        LCompilers::LLVMEvaluator e;
        std::string ir = rsqrt_sum_dp_ir;
        ir.replace(ir.find("+avx512f,+avx512vl"),
            std::string("+avx512f,+avx512vl").size(), "+avx2");
        std::unique_ptr<llvm::Module> m = e.parse_module(ir, "");
        llvm::Function &f = *m->getFunction("rsqrt_sum_dp");
        CHECK(!LCompilers::expand_reciprocal_sqrt(f,
            llvm::Triple("x86_64-unknown-linux-gnu")));
        CHECK(!calls_intrinsic(f, "llvm.x86.avx512.rsqrt14.pd.512"));
    }
}
#endif // __EMSCRIPTEN__

// Whether the LLVM IR defines (not merely declares) a function whose
// definition line mentions `name`.
static bool llvm_ir_defines(const std::string &ir, const std::string &name) {
    size_t pos = 0;
    while ((pos = ir.find("define ", pos)) != std::string::npos) {
        size_t end = ir.find('\n', pos);
        std::string line = ir.substr(pos, end == std::string::npos
            ? std::string::npos : end - pos);
        if (line.find(name) != std::string::npos) return true;
        if (end == std::string::npos) break;
        pos = end;
    }
    return false;
}

TEST_CASE("FortranEvaluator keeps only linkable user procedures across cells") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    // A function with an assumed-shape dummy is specialized by
    // array_by_data (total_16858_real____0). Its body's `sum(x)` also
    // instantiates a compiler-generated helper, which is specialized in
    // turn. A later cell can link to the user function, but not to the
    // helper, which it regenerates for itself. Only the user function's
    // descriptor original may stay in this cell's IR.
    LCompilers::LocationManager lm;
    LCompilers::PassManager lpm;
    lpm.use_default_passes();
    lpm.skip_llvm_backend_passes();
    LCompilers::diag::Diagnostics d;
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate(
        R"(real(8) function total_16858(x)
real(8), intent(in) :: x(:)
total_16858 = sum(x)
end function
)", true, lm, lpm, d);
    INFO(d.render2());
    REQUIRE(r.ok);
    const std::string &ir = r.result.llvm_ir;
    CHECK(llvm_ir_defines(ir, "total_16858("));
    CHECK(llvm_ir_defines(ir, "_lcompilers_Sum_8_1_0_idx4_real____0("));
    // Match the helper's name only, so the assertion does not depend on how
    // LLVM spells the descriptor argument.
    CHECK_FALSE(llvm_ir_defines(ir, "_lcompilers_Sum_8_1_0_idx4("));
    // The retained descriptor original still links from a later cell. Use a
    // section, not a contiguous constructor: only a section keeps the
    // descriptor call that needs the original.
    REQUIRE(e.evaluate2(R"(real(8) :: w_16858(3)
w_16858 = [1.0d0, 2.0d0, 3.0d0]
)").ok);
    LCompilers::Result<FortranEvaluator::EvalResult> r2
        = e.evaluate2("total_16858(w_16858(2:3))");
    REQUIRE(r2.ok);
    CHECK(r2.result.type == FortranEvaluator::EvalResult::real8);
    CHECK(r2.result.f64 == 5.0);
}

TEST_CASE("FortranEvaluator drops only descriptor originals of nested procedures") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    // The scope half of the retention rule: a top-level procedure's descriptor
    // original is kept for later cells, while an internal procedure's is
    // dropped because no other unit can reach it. Both still get a data
    // version that the host body calls.
    LCompilers::LocationManager lm;
    LCompilers::PassManager lpm;
    lpm.use_default_passes();
    lpm.skip_llvm_backend_passes();
    LCompilers::diag::Diagnostics d;
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate(
        R"(real(8) function outer_16858(x)
real(8), intent(in) :: x(:)
outer_16858 = inner_16858(x)
contains
real(8) function inner_16858(y)
real(8), intent(in) :: y(:)
inner_16858 = sum(y)
end function
end function
)", true, lm, lpm, d);
    INFO(d.render2());
    REQUIRE(r.ok);
    const std::string &ir = r.result.llvm_ir;
    CHECK(llvm_ir_defines(ir, "outer_16858("));
    CHECK(llvm_ir_defines(ir, "outer_16858.inner_16858_real____0("));
    CHECK_FALSE(llvm_ir_defines(ir, ".inner_16858("));
    // The host original still links from a later cell with a section.
    REQUIRE(e.evaluate2(R"(real(8) :: v_16858(4)
v_16858 = [1.0d0, 2.0d0, 3.0d0, 4.0d0]
)").ok);
    LCompilers::Result<FortranEvaluator::EvalResult> r2
        = e.evaluate2("outer_16858(v_16858(2:3))");
    REQUIRE(r2.ok);
    CHECK(r2.result.type == FortranEvaluator::EvalResult::real8);
    CHECK(r2.result.f64 == 5.0);
}

TEST_CASE("FortranEvaluator keeps a user procedure with a helper-like name") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    // Compiler provenance is structural, not a name prefix: a user procedure
    // may legitimately start with `_lcompilers_`. Its descriptor original must
    // stay so a later cell can call it with a section.
    LCompilers::LocationManager lm;
    LCompilers::PassManager lpm;
    lpm.use_default_passes();
    lpm.skip_llvm_backend_passes();
    LCompilers::diag::Diagnostics d;
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate(
        R"(real(8) function _lcompilers_user_16858(x)
real(8), intent(in) :: x(:)
_lcompilers_user_16858 = sum(x)
end function
)", true, lm, lpm, d);
    INFO(d.render2());
    REQUIRE(r.ok);
    const std::string &ir = r.result.llvm_ir;
    CHECK(llvm_ir_defines(ir, "_lcompilers_user_16858("));
    REQUIRE(e.evaluate2(R"(real(8) :: vv2_16858(4)
vv2_16858 = [1.0d0, 2.0d0, 3.0d0, 4.0d0]
)").ok);
    LCompilers::Result<FortranEvaluator::EvalResult> r2
        = e.evaluate2("_lcompilers_user_16858(vv2_16858(2:3))");
    REQUIRE(r2.ok);
    CHECK(r2.result.type == FortranEvaluator::EvalResult::real8);
    CHECK(r2.result.f64 == 5.0);
}

TEST_CASE("FortranEvaluator redefines a procedure in a later cell") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    {
        FortranEvaluator e(cu);
        REQUIRE(e.evaluate2(R"(integer function value_16858(x)
integer, intent(in) :: x
value_16858 = x + 1
end function
)").ok);
        LCompilers::Result<FortranEvaluator::EvalResult> r
            = e.evaluate2("value_16858(10)");
        REQUIRE(r.ok);
        CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
        CHECK(r.result.i32 == 11);
        // A later definition of the same name shadows the earlier one for
        // cells that come after it.
        REQUIRE(e.evaluate2(R"(integer function value_16858(x)
integer, intent(in) :: x
value_16858 = x * 10
end function
)").ok);
        r = e.evaluate2("value_16858(10)");
        REQUIRE(r.ok);
        CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
        CHECK(r.result.i32 == 100);
    }
    {
        FortranEvaluator e(cu);
        REQUIRE(e.evaluate2(R"(module m_red_16858
implicit none
contains
integer function value_16858(x)
integer, intent(in) :: x
value_16858 = x + 1
end function
end module
)").ok);
        LCompilers::Result<FortranEvaluator::EvalResult> r
            = e.evaluate2("use m_red_16858\nvalue_16858(10)\n");
        REQUIRE(r.ok);
        CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
        CHECK(r.result.i32 == 11);
        // Redefining the module and using it in a later cell sees the new
        // module procedure body.
        REQUIRE(e.evaluate2(R"(module m_red_16858
implicit none
contains
integer function value_16858(x)
integer, intent(in) :: x
value_16858 = x * 10
end function
end module
)").ok);
        r = e.evaluate2("use m_red_16858\nvalue_16858(10)\n");
        REQUIRE(r.ok);
        CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
        CHECK(r.result.i32 == 100);
    }
    {
        FortranEvaluator e(cu);
        // An assumed-shape procedure is specialized by array_by_data, so
        // redefining it also exercises the pass's specialization cache.
        REQUIRE(e.evaluate2(R"(real(8) function arr_value_16858(x)
real(8), intent(in) :: x(:)
arr_value_16858 = x(1) + x(2)
end function
)").ok);
        REQUIRE(e.evaluate2(R"(real(8) :: av_16858(2)
av_16858 = [1.0d0, 2.0d0]
)").ok);
        LCompilers::Result<FortranEvaluator::EvalResult> r
            = e.evaluate2("arr_value_16858(av_16858)");
        REQUIRE(r.ok);
        CHECK(r.result.type == FortranEvaluator::EvalResult::real8);
        CHECK(r.result.f64 == 3.0);
        // The redefined body is used; the cached specialization of the first
        // definition is not reused.
        REQUIRE(e.evaluate2(R"(real(8) function arr_value_16858(x)
real(8), intent(in) :: x(:)
arr_value_16858 = 2.0d0 * (x(1) + x(2))
end function
)").ok);
        r = e.evaluate2("arr_value_16858(av_16858)");
        REQUIRE(r.ok);
        CHECK(r.result.type == FortranEvaluator::EvalResult::real8);
        CHECK(r.result.f64 == 6.0);
    }
}

TEST_CASE("ASR structure constructor with an allocatable component in an assignment") {
    // An initialization expression cannot give an allocatable component a
    // value, but an assignment may: it allocates the component. The check
    // for the initialization-expression restriction must not fire here.
    std::string source = R"(module alloc_component_assignment_m
implicit none
type :: inner_t
    integer :: n
end type
type :: outer_t
    type(inner_t), allocatable :: c(:)
end type
type :: nested_inner_t
    integer, allocatable :: a
end type
type :: nested_outer_t
    type(nested_inner_t) :: arr(1)
end type
contains
subroutine assign_alloc_component()
    type(outer_t) :: x
    x = outer_t([inner_t(5)])
    x = outer_t(null())
    x = outer_t()
end subroutine
subroutine assign_nested_alloc_component()
    type(nested_outer_t) :: y
    y = nested_outer_t([nested_inner_t(5)])
end subroutine
end module)";
    Allocator al(256*1024);
    LCompilers::diag::Diagnostics diagnostics;
    CompilerOptions compiler_options;
    compiler_options.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
    auto *ast = TRY(LCompilers::LFortran::parse(al, source,
        diagnostics, compiler_options));
    LCompilers::LocationManager lm;
    auto *asr = TRY(LCompilers::LFortran::ast_to_asr(al, *ast, diagnostics,
        nullptr, false, compiler_options, lm));
    CHECK_FALSE(diagnostics.has_error());
    CHECK(asr->m_symtab->get_symbol("alloc_component_assignment_m") != nullptr);
}

TEST_CASE("FortranEvaluator internal assumed-shape procedure as an actual procedure argument") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    // An earlier cell defines a procedure whose dummy procedure has an
    // assumed-shape dummy; array_by_data compiles that cell's procedure to
    // take the dummy procedure by data. A later cell passes an internal
    // procedure as the actual argument, which array_by_data specializes and
    // passes by data. The later cell has to declare the earlier procedure
    // with the same by-data dummy that its own cell compiled.
    REQUIRE(e.evaluate2(R"(subroutine apply_16946(f, r)
interface
real(8) function f(x)
real(8), intent(in) :: x(:)
end function
end interface
real(8), intent(out) :: r
r = f([1.0d0, 2.0d0])
end subroutine
)").ok);
    REQUIRE(e.evaluate2(R"(subroutine host_16946(r)
real(8), intent(out) :: r
call apply_16946(inner, r)
contains
real(8) function inner(x)
real(8), intent(in) :: x(:)
inner = sum(x)
end function
end subroutine
)").ok);
    REQUIRE(e.evaluate2("real(8) :: rr_16946").ok);
    REQUIRE(e.evaluate2("call host_16946(rr_16946)").ok);
    LCompilers::Result<FortranEvaluator::EvalResult> r
        = e.evaluate2("rr_16946");
    REQUIRE(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::real8);
    CHECK(r.result.f64 == 3.0);
}

TEST_CASE("FortranEvaluator earlier top-level assumed-shape procedure as an actual procedure argument") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    // A top-level procedure of an earlier cell passed as the actual
    // argument to an earlier cell's procedure has to be passed by data:
    // both cells compiled their procedures with by-data interfaces.
    REQUIRE(e.evaluate2(R"(subroutine apply_t_16946(f, r)
interface
real(8) function f(x)
real(8), intent(in) :: x(:)
end function
end interface
real(8), intent(out) :: r
r = f([1.0d0, 2.0d0])
end subroutine
)").ok);
    REQUIRE(e.evaluate2(R"(real(8) function twice_16946(x)
real(8), intent(in) :: x(:)
twice_16946 = 2.0d0 * sum(x)
end function
)").ok);
    REQUIRE(e.evaluate2("real(8) :: rt_16946").ok);
    REQUIRE(e.evaluate2("call apply_t_16946(twice_16946, rt_16946)").ok);
    LCompilers::Result<FortranEvaluator::EvalResult> r
        = e.evaluate2("rt_16946");
    REQUIRE(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::real8);
    CHECK(r.result.f64 == 6.0);
}

TEST_CASE("FortranEvaluator module procedure taking a procedure argument") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    REQUIRE(e.evaluate2(R"(module m_16946
implicit none
contains
subroutine apply_m_16946(f, r)
interface
real(8) function f(x)
real(8), intent(in) :: x(:)
end function
end interface
real(8), intent(out) :: r
r = f([1.0d0, 2.0d0, 3.0d0])
end subroutine
end module
)").ok);
    REQUIRE(e.evaluate2(R"(subroutine host_m_16946(r)
use m_16946
real(8), intent(out) :: r
call apply_m_16946(inner, r)
contains
real(8) function inner(x)
real(8), intent(in) :: x(:)
inner = sum(x)
end function
end subroutine
)").ok);
    REQUIRE(e.evaluate2("real(8) :: rm_16946").ok);
    REQUIRE(e.evaluate2("call host_m_16946(rm_16946)").ok);
    LCompilers::Result<FortranEvaluator::EvalResult> r
        = e.evaluate2("rm_16946");
    REQUIRE(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::real8);
    CHECK(r.result.f64 == 6.0);
}

TEST_CASE("FortranEvaluator character variables across cells") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2(
        "character(len=20) :: greeting\n"
        "greeting = \"hello\"\n");
    REQUIRE(r.ok);
    r = e.evaluate2("greeting");
    REQUIRE(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::character);
    CHECK(r.result.str == "hello               ");
    r = e.evaluate2("greeting = \"bye\"");
    REQUIRE(r.ok);
    r = e.evaluate2("trim(greeting) // \"!\"");
    REQUIRE(r.ok);
    CHECK(r.result.str == "bye!");

    r = e.evaluate2("character(len=3) :: initialized = \"abc\"");
    REQUIRE(r.ok);
    r = e.evaluate2("initialized");
    REQUIRE(r.ok);
    CHECK(r.result.str == "abc");

    r = e.evaluate2("character(len=5) :: names(2)");
    REQUIRE(r.ok);
    r = e.evaluate2("names(1) = \"ab\"\nnames(2) = \"cdefg\"");
    REQUIRE(r.ok);
    r = e.evaluate2("names(1) // names(2)");
    REQUIRE(r.ok);
    CHECK(r.result.str == "ab   cdefg");

    r = e.evaluate2("character(len=2) :: pairs(2) = [\"xy\", \"zw\"]");
    REQUIRE(r.ok);
    r = e.evaluate2("pairs(2) // pairs(1)");
    REQUIRE(r.ok);
    CHECK(r.result.str == "zwxy");

    r = e.evaluate2("character(len=:), allocatable :: deferred");
    REQUIRE(r.ok);
    r = e.evaluate2("deferred = \"grows\"");
    REQUIRE(r.ok);
    r = e.evaluate2("deferred // \"!\"");
    REQUIRE(r.ok);
    CHECK(r.result.str == "grows!");

    // A procedure of a later cell assigns to the global.
    r = e.evaluate2("subroutine set_greeting()\ngreeting = \"done\"\nend subroutine");
    REQUIRE(r.ok);
    r = e.evaluate2("call set_greeting()");
    REQUIRE(r.ok);
    r = e.evaluate2("trim(greeting)");
    REQUIRE(r.ok);
    CHECK(r.result.str == "done");
}

TEST_CASE("FortranEvaluator module character variables across cells") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2(
        "module char_vars_13453\n"
        "character(len=3) :: label = \"mod\"\n"
        "character(len=2) :: tags(2)\n"
        "end module\n");
    REQUIRE(r.ok);
    r = e.evaluate2("use char_vars_13453\ntags(1) = \"q1\"");
    REQUIRE(r.ok);
    r = e.evaluate2("use char_vars_13453\nlabel // tags(1)");
    REQUIRE(r.ok);
    CHECK(r.result.str == "modq1");
}

TEST_CASE("FortranEvaluator array initializer of a different shape") {
    // `integer :: xi(3) = [1, 2]` is rejected in semantics, not in codegen,
    // and leaves the session usable.
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::LocationManager lm;
    LCompilers::PassManager lpm;
    lpm.use_default_passes();
    LCompilers::diag::Diagnostics d;
    LCompilers::Result<FortranEvaluator::EvalResult> r
        = e.evaluate("integer :: xi_16971(3) = [1, 2]", false, lm, lpm, d);
    CHECK(!r.ok);
    REQUIRE(d.diagnostics.size() >= 1);
    CHECK(d.diagnostics[0].stage == LCompilers::diag::Stage::Semantic);
    CHECK(d.diagnostics[0].message
        == "different shape for array initialization on dimension 1 (3 and 2)");
    REQUIRE(e.evaluate2("integer :: yi_16971(3) = [1, 2, 3]").ok);
    r = e.evaluate2("sum(yi_16971)");
    REQUIRE(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 6);
}

// A global declared in a cell with the TYPE(intrinsic-type-spec) form keeps
// its initializer in later cells, as one declared with `integer` does
// (#16972, #16973).
TEST_CASE("FortranEvaluator type(intrinsic-type-spec) global initializer") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);

    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2("type(integer) :: sc_16972 = 5");
    REQUIRE(r.ok);
    r = e.evaluate2("sc_16972");
    REQUIRE(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 5);
    r = e.evaluate2("type(integer) :: tj_16972(2) = 5");
    REQUIRE(r.ok);
    r = e.evaluate2("tj_16972(1) + 10 * tj_16972(2)");
    REQUIRE(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 55);
    r = e.evaluate2("type(integer) :: ti_16973(3) = [1, 2, 3]");
    REQUIRE(r.ok);
    r = e.evaluate2("ti_16973(1) + 10 * ti_16973(2) + 100 * ti_16973(3)");
    REQUIRE(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 321);
}

// The parent component of an extended type (#13073), named after the parent
// type, written and read in one cell and across cells.
TEST_CASE("FortranEvaluator parent component reference") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);

    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2(R"(type :: point_13073
integer :: x, y
end type
type, extends(point_13073) :: p3_13073
integer :: z
end type
type(p3_13073) :: q_13073
type(point_13073) :: pp_13073
q_13073%point_13073%x = 4
q_13073%y = 5
pp_13073 = q_13073%point_13073
)");
    REQUIRE(r.ok);
    r = e.evaluate2("pp_13073%x + pp_13073%y");
    REQUIRE(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 9);
    r = e.evaluate2("q_13073%point_13073 = point_13073(6, 7)");
    REQUIRE(r.ok);
    r = e.evaluate2("q_13073%point_13073%x + q_13073%y");
    REQUIRE(r.ok);
    CHECK(r.result.type == FortranEvaluator::EvalResult::integer4);
    CHECK(r.result.i32 == 13);
}

// Each cell's module defines its own internal finalizer functions, under the
// same names as those of earlier cells (#14395).
TEST_CASE("FortranEvaluator finalizers in several cells") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    // Allocatable array temporaries.
    LCompilers::Result<FortranEvaluator::EvalResult> r = e.evaluate2("integer :: i");
    REQUIRE(r.ok);
    REQUIRE(e.evaluate2("print \"(i0)\", (i, i = 1, 3)").ok);
    REQUIRE(e.evaluate2("print \"(i0)\", (i, i = 1, 5)").ok);
    REQUIRE(e.evaluate2("real(8) :: a(3, 0:2), b(3, 0:2)\na = 1\nb = 2").ok);
    r = e.evaluate2("norm2(a(:, 0) - b(:, 0))");
    REQUIRE(r.ok);
    CHECK(std::abs(r.result.f64 - std::sqrt(3.0)) < 1e-14);
    r = e.evaluate2("norm2(a(:, 1) - b(:, 1))");
    REQUIRE(r.ok);
    CHECK(std::abs(r.result.f64 - std::sqrt(3.0)) < 1e-14);
    REQUIRE(e.evaluate2("real(8) :: c\nc = norm2(a(:, 2) - b(:, 2))").ok);
    r = e.evaluate2("c");
    REQUIRE(r.ok);
    CHECK(std::abs(r.result.f64 - std::sqrt(3.0)) < 1e-14);
    REQUIRE(e.evaluate2("real(8) :: d(0:2)").ok);
    REQUIRE(e.evaluate2("d = sqrt(sum((a - b)**2, dim=1))").ok);
    r = e.evaluate2("sum(d)");
    REQUIRE(r.ok);
    CHECK(std::abs(r.result.f64 - 3 * std::sqrt(3.0)) < 1e-14);

    // The finalizer of a type with an allocatable component.
    REQUIRE(e.evaluate2(R"(type :: t_14395
    integer, allocatable :: x(:)
end type
type(t_14395), allocatable :: q
)").ok);
    for (int k = 0; k < 2; k++) {
        REQUIRE(e.evaluate2("allocate(q)\nallocate(q%x(3))\n"
            "q%x = [1, 2, 3]").ok);
        r = e.evaluate2("sum(q%x)");
        REQUIRE(r.ok);
        CHECK(r.result.i32 == 6);
        REQUIRE(e.evaluate2("if (allocated(q)) deallocate(q)").ok);
        r = e.evaluate2("allocated(q)");
        REQUIRE(r.ok);
        CHECK(r.result.b == false);
    }
}

// Class pointers across cells hang on Windows, as in the tests of
// https://github.com/lfortran/lfortran/pull/14350 above.
#if !defined(_WIN32)
TEST_CASE("FortranEvaluator finalizers of class pointers in several cells") {
    CompilerOptions cu;
    cu.interactive = true;
    cu.po.runtime_library_dir = LCompilers::LFortran::get_runtime_library_dir();
    FortranEvaluator e(cu);
    LCompilers::Result<FortranEvaluator::EvalResult>
    r = e.evaluate2(R"(module m_14395
implicit none
type :: s
    integer :: i = 0
end type
class(s), pointer :: p => null()
end module
)");
    REQUIRE(r.ok);
    REQUIRE(e.evaluate2("use m_14395").ok);
    // A top-level `deallocate(p)` is taken for a function call (#13455).
    REQUIRE(e.evaluate2("if (associated(p)) deallocate(p)").ok);
    REQUIRE(e.evaluate2("if (associated(p)) deallocate(p)").ok);
    r = e.evaluate2("associated(p)");
    REQUIRE(r.ok);
    CHECK(r.result.b == false);

    REQUIRE(e.evaluate2(R"(type :: t_14395
    integer, allocatable :: x(:)
end type
class(t_14395), pointer :: q => null()
)").ok);
    for (int k = 0; k < 2; k++) {
        REQUIRE(e.evaluate2("if (.not. associated(q)) allocate(q)\n"
            "allocate(q%x(3))\nq%x = [1, 2, 3]").ok);
        r = e.evaluate2("sum(q%x)");
        REQUIRE(r.ok);
        CHECK(r.result.i32 == 6);
        REQUIRE(e.evaluate2("if (associated(q)) deallocate(q)").ok);
        r = e.evaluate2("associated(q)");
        REQUIRE(r.ok);
        CHECK(r.result.b == false);
    }
}
#endif

#ifndef __EMSCRIPTEN__
namespace {

// A fast three-term sum of squares of four vectors, written the way the SLP
// vectorizer writes it: three shuffle chains gather one component of the four
// vectors and the squares are added. The horizontal reduction pass rewrites
// this as two horizontal adds, two shuffles and one add.
const std::string reductions_sum_ir = R"""(
define double @reductions_sum() #0 {
entry:
  %v0 = fsub fast <4 x double> <double 1.0, double 2.0, double 3.0, double 0.0>, zeroinitializer
  %v1 = fsub fast <4 x double> <double 2.0, double 3.0, double 4.0, double 0.0>, zeroinitializer
  %v2 = fsub fast <4 x double> <double 3.0, double 4.0, double 5.0, double 0.0>, zeroinitializer
  %v3 = fsub fast <4 x double> <double 4.0, double 5.0, double 6.0, double 0.0>, zeroinitializer
  %g0a = shufflevector <4 x double> %v0, <4 x double> %v1, <4 x i32> <i32 0, i32 4, i32 undef, i32 undef>
  %g0b = shufflevector <4 x double> %g0a, <4 x double> %v2, <4 x i32> <i32 0, i32 1, i32 4, i32 undef>
  %g0c = shufflevector <4 x double> %g0b, <4 x double> %v3, <4 x i32> <i32 0, i32 1, i32 2, i32 4>
  %s0 = fmul fast <4 x double> %g0c, %g0c
  %g1a = shufflevector <4 x double> %v0, <4 x double> %v1, <4 x i32> <i32 1, i32 5, i32 undef, i32 undef>
  %g1b = shufflevector <4 x double> %g1a, <4 x double> %v2, <4 x i32> <i32 0, i32 1, i32 5, i32 undef>
  %g1c = shufflevector <4 x double> %g1b, <4 x double> %v3, <4 x i32> <i32 0, i32 1, i32 2, i32 5>
  %s1 = fmul fast <4 x double> %g1c, %g1c
  %g2a = shufflevector <4 x double> %v0, <4 x double> %v1, <4 x i32> <i32 2, i32 6, i32 undef, i32 undef>
  %g2b = shufflevector <4 x double> %g2a, <4 x double> %v2, <4 x i32> <i32 0, i32 1, i32 6, i32 undef>
  %g2c = shufflevector <4 x double> %g2b, <4 x double> %v3, <4 x i32> <i32 0, i32 1, i32 2, i32 6>
  %s2 = fmul fast <4 x double> %g2c, %g2c
  %t0 = fadd fast <4 x double> %s0, %s1
  %t1 = fadd fast <4 x double> %t0, %s2
  %r = extractelement <4 x double> %t1, i32 0
  ret double %r
}
attributes #0 = { "target-features"="+avx" }
)""";

// A call to an intrinsic whose name contains `name`.
bool calls_intrinsic_like(llvm::Function &f, const std::string &name) {
    for (llvm::BasicBlock &bb : f) {
        for (llvm::Instruction &i : bb) {
            auto *call = llvm::dyn_cast<llvm::CallInst>(&i);
            if (!call) {
                continue;
            }
            llvm::Function *callee = call->getCalledFunction();
            if (callee && std::string(callee->getName()).find(name)
                    != std::string::npos) {
                return true;
            }
        }
    }
    return false;
}

// A target machine for `triple` with the baseline `cpu` and no extra
// features, so the function attributes decide the subtarget. Null if LLVM
// was built without that target.
std::unique_ptr<llvm::TargetMachine> reductions_target_machine(
        const std::string &triple, const std::string &cpu) {
    LCompilers::LLVMEvaluator e; // Registers the targets LFortran supports.
    std::string error;
#if LLVM_VERSION_MAJOR >= 21
    const llvm::Target *target = llvm::TargetRegistry::lookupTarget(
        llvm::Triple(triple), error);
#else
    const llvm::Target *target = llvm::TargetRegistry::lookupTarget(
        triple, error);
#endif
    if (!target) {
        return nullptr;
    }
    llvm::TargetOptions options;
    return std::unique_ptr<llvm::TargetMachine>(target->createTargetMachine(
#if LLVM_VERSION_MAJOR >= 21
        llvm::Triple(triple),
#else
        triple,
#endif
        cpu, "", options, llvm::Reloc::PIC_));
}

// Whether the pass rewrites `reductions_sum` in `ir` for `tm`. Checks that
// the result is valid IR and calls the horizontal add exactly when rewritten.
bool reductions_rewrite(const std::string &ir, const llvm::TargetMachine &tm) {
    LCompilers::LLVMEvaluator e;
    std::unique_ptr<llvm::Module> m = e.parse_module(ir, "");
    llvm::Function &f = *m->getFunction("reductions_sum");
    bool changed = LCompilers::recombine_horizontal_reductions(f, tm);
    std::string err;
    llvm::raw_string_ostream os(err);
    CHECK(!llvm::verifyFunction(f, &os));
    CHECK(calls_intrinsic_like(f, "hadd") == changed);
    return changed;
}

#if LLVM_VERSION_MAJOR >= 11
// The same sum of squares in a loop. BASES defines the four vectors %v0 to
// %v3 from the loop-carried vector %x and the loaded vectors %la and %lb,
// and UPDATE defines %x.next. The pass clears the fourth component unless it
// can prove that it is zero in all four vectors.
const std::string reductions_loop_ir = R"""(
define double @reductions_sum(<4 x double>* %p, i32 %n) #0 {
entry:
  %la = load <4 x double>, <4 x double>* %p
  %q = getelementptr <4 x double>, <4 x double>* %p, i64 1
  %lb = load <4 x double>, <4 x double>* %q
  br label %loop

loop:
  %x = phi <4 x double> [ <double 1.0, double 2.0, double 3.0, double 0.0>, %entry ], [ %x.next, %loop ]
  %i = phi i32 [ 0, %entry ], [ %i.next, %loop ]
BASES
  %g0a = shufflevector <4 x double> %v0, <4 x double> %v1, <4 x i32> <i32 0, i32 4, i32 undef, i32 undef>
  %g0b = shufflevector <4 x double> %g0a, <4 x double> %v2, <4 x i32> <i32 0, i32 1, i32 4, i32 undef>
  %g0c = shufflevector <4 x double> %g0b, <4 x double> %v3, <4 x i32> <i32 0, i32 1, i32 2, i32 4>
  %s0 = fmul fast <4 x double> %g0c, %g0c
  %g1a = shufflevector <4 x double> %v0, <4 x double> %v1, <4 x i32> <i32 1, i32 5, i32 undef, i32 undef>
  %g1b = shufflevector <4 x double> %g1a, <4 x double> %v2, <4 x i32> <i32 0, i32 1, i32 5, i32 undef>
  %g1c = shufflevector <4 x double> %g1b, <4 x double> %v3, <4 x i32> <i32 0, i32 1, i32 2, i32 5>
  %s1 = fmul fast <4 x double> %g1c, %g1c
  %g2a = shufflevector <4 x double> %v0, <4 x double> %v1, <4 x i32> <i32 2, i32 6, i32 undef, i32 undef>
  %g2b = shufflevector <4 x double> %g2a, <4 x double> %v2, <4 x i32> <i32 0, i32 1, i32 6, i32 undef>
  %g2c = shufflevector <4 x double> %g2b, <4 x double> %v3, <4 x i32> <i32 0, i32 1, i32 2, i32 6>
  %s2 = fmul fast <4 x double> %g2c, %g2c
  %t0 = fadd fast <4 x double> %s0, %s1
  %t1 = fadd fast <4 x double> %t0, %s2
UPDATE
  %i.next = add i32 %i, 1
  %c = icmp slt i32 %i.next, %n
  br i1 %c, label %loop, label %exit

exit:
  %r = extractelement <4 x double> %t1, i32 0
  ret double %r
}
attributes #0 = { "target-features"="+avx" }
)""";

// The clear of lane 3: shuffle lanes 0, 1 and 2 from the square and lane 3
// from a constant zero vector. A bitwise and would leave a poison lane
// poison, and vhaddpd would then read it.
bool clears_lane3(const llvm::Instruction &i) {
    auto *shuffle = llvm::dyn_cast<llvm::ShuffleVectorInst>(&i);
    if (!shuffle) {
        return false;
    }
    llvm::ArrayRef<int> mask = shuffle->getShuffleMask();
    if (mask.size() != 4 || mask[0] != 0 || mask[1] != 1 || mask[2] != 2
            || mask[3] != 4) {
        return false;
    }
    auto *zero = llvm::dyn_cast<llvm::Constant>(shuffle->getOperand(1));
    return zero && zero->isNullValue();
}

// Whether the pass rewrites `reductions_loop_ir` with the given bases and
// update for `tm`, and replaces the fourth component with a constant zero.
bool reductions_loop_masks(const std::string &bases,
        const std::string &update, const llvm::TargetMachine &tm) {
    std::string ir = reductions_loop_ir;
    ir.replace(ir.find("BASES"), 5, bases);
    ir.replace(ir.find("UPDATE"), 6, update);
    LCompilers::LLVMEvaluator e;
    std::unique_ptr<llvm::Module> m = e.parse_module(ir, "");
    llvm::Function &f = *m->getFunction("reductions_sum");
    REQUIRE(LCompilers::recombine_horizontal_reductions(f, tm));
    std::string err;
    llvm::raw_string_ostream os(err);
    CHECK(!llvm::verifyFunction(f, &os));
    bool masked = false;
    for (llvm::BasicBlock &bb : f) {
        for (llvm::Instruction &i : bb) {
            masked = masked || clears_lane3(i);
        }
    }
    return masked;
}

// The same square added twice, with no pure three-term subtree:
// (s0+s1)+(s0+s2) is 2*s0+s1+s2. Lane 0 is 2*1^2+2^2+3^2 = 15.
// A shared value is not a tree, so the pass must leave it unchanged.
const std::string reductions_diamond_ir = R"""(
define double @reductions_sum() #0 {
entry:
  %v0 = fsub fast <4 x double> <double 1.0, double 2.0, double 3.0, double 0.0>, zeroinitializer
  %v1 = fsub fast <4 x double> <double 2.0, double 3.0, double 4.0, double 0.0>, zeroinitializer
  %v2 = fsub fast <4 x double> <double 3.0, double 4.0, double 5.0, double 0.0>, zeroinitializer
  %v3 = fsub fast <4 x double> <double 4.0, double 5.0, double 6.0, double 0.0>, zeroinitializer
  %g0a = shufflevector <4 x double> %v0, <4 x double> %v1, <4 x i32> <i32 0, i32 4, i32 undef, i32 undef>
  %g0b = shufflevector <4 x double> %g0a, <4 x double> %v2, <4 x i32> <i32 0, i32 1, i32 4, i32 undef>
  %g0c = shufflevector <4 x double> %g0b, <4 x double> %v3, <4 x i32> <i32 0, i32 1, i32 2, i32 4>
  %s0 = fmul fast <4 x double> %g0c, %g0c
  %g1a = shufflevector <4 x double> %v0, <4 x double> %v1, <4 x i32> <i32 1, i32 5, i32 undef, i32 undef>
  %g1b = shufflevector <4 x double> %g1a, <4 x double> %v2, <4 x i32> <i32 0, i32 1, i32 5, i32 undef>
  %g1c = shufflevector <4 x double> %g1b, <4 x double> %v3, <4 x i32> <i32 0, i32 1, i32 2, i32 5>
  %s1 = fmul fast <4 x double> %g1c, %g1c
  %g2a = shufflevector <4 x double> %v0, <4 x double> %v1, <4 x i32> <i32 2, i32 6, i32 undef, i32 undef>
  %g2b = shufflevector <4 x double> %g2a, <4 x double> %v2, <4 x i32> <i32 0, i32 1, i32 6, i32 undef>
  %g2c = shufflevector <4 x double> %g2b, <4 x double> %v3, <4 x i32> <i32 0, i32 1, i32 2, i32 6>
  %s2 = fmul fast <4 x double> %g2c, %g2c
  %s01 = fadd fast <4 x double> %s0, %s1
  %s02 = fadd fast <4 x double> %s0, %s2
  %t2 = fadd fast <4 x double> %s01, %s02
  %r = extractelement <4 x double> %t2, i32 0
  ret double %r
}
attributes #0 = { "target-features"="+avx" }
)""";

// Same gathers, with a nonzero lane 3. The result is
// 1*v0 + 2*v1 + 4*v2 + 8*v3 of the three-component sums: 888.
// Counting lane 3 as well would be 1581. The freeze keeps the rewritten
// vectors from folding to constants, so the clear of lane 3 stays visible.
const std::string reductions_lanes_ir = R"""(
define double @reductions_lanes() #0 {
entry:
  %z = freeze <4 x double> zeroinitializer
  %v0 = fsub fast <4 x double> <double 1.0, double 2.0, double 3.0, double 9.0>, %z
  %v1 = fsub fast <4 x double> <double 2.0, double 3.0, double 4.0, double 8.0>, %z
  %v2 = fsub fast <4 x double> <double 3.0, double 4.0, double 5.0, double 7.0>, %z
  %v3 = fsub fast <4 x double> <double 4.0, double 5.0, double 6.0, double 6.0>, %z
  %g0a = shufflevector <4 x double> %v0, <4 x double> %v1, <4 x i32> <i32 0, i32 4, i32 undef, i32 undef>
  %g0b = shufflevector <4 x double> %g0a, <4 x double> %v2, <4 x i32> <i32 0, i32 1, i32 4, i32 undef>
  %g0c = shufflevector <4 x double> %g0b, <4 x double> %v3, <4 x i32> <i32 0, i32 1, i32 2, i32 4>
  %s0 = fmul fast <4 x double> %g0c, %g0c
  %g1a = shufflevector <4 x double> %v0, <4 x double> %v1, <4 x i32> <i32 1, i32 5, i32 undef, i32 undef>
  %g1b = shufflevector <4 x double> %g1a, <4 x double> %v2, <4 x i32> <i32 0, i32 1, i32 5, i32 undef>
  %g1c = shufflevector <4 x double> %g1b, <4 x double> %v3, <4 x i32> <i32 0, i32 1, i32 2, i32 5>
  %s1 = fmul fast <4 x double> %g1c, %g1c
  %g2a = shufflevector <4 x double> %v0, <4 x double> %v1, <4 x i32> <i32 2, i32 6, i32 undef, i32 undef>
  %g2b = shufflevector <4 x double> %g2a, <4 x double> %v2, <4 x i32> <i32 0, i32 1, i32 6, i32 undef>
  %g2c = shufflevector <4 x double> %g2b, <4 x double> %v3, <4 x i32> <i32 0, i32 1, i32 2, i32 6>
  %s2 = fmul fast <4 x double> %g2c, %g2c
  %t0 = fadd fast <4 x double> %s0, %s1
  %t1 = fadd fast <4 x double> %t0, %s2
  %e0 = extractelement <4 x double> %t1, i32 0
  %e1 = extractelement <4 x double> %t1, i32 1
  %e2 = extractelement <4 x double> %t1, i32 2
  %e3 = extractelement <4 x double> %t1, i32 3
  %w1 = fmul double %e1, 2.0
  %w2 = fmul double %e2, 4.0
  %w3 = fmul double %e3, 8.0
  %a = fadd double %e0, %w1
  %b = fadd double %w2, %w3
  %r = fadd double %a, %b
  ret double %r
}
attributes #0 = { "target-features"="+avx" }
)""";

// Lane 3 starts at 0 and is multiplied by +Inf. The gathers never read it,
// so lane 0 of the last iteration is the first vector's 0^2+1^2+2^2 = 5.
// Folding the Inf into vhaddpd would make that lane NaN.
const std::string reductions_inf_ir = R"""(
define double @reductions_inf() #0 {
entry:
  br label %loop

loop:
  %x = phi <4 x double> [ <double 1.0, double 2.0, double 3.0, double 0.0>, %entry ], [ %x.next, %loop ]
  %i = phi i32 [ 0, %entry ], [ %i.next, %loop ]
  %v0 = fsub fast <4 x double> %x, <double 1.0, double 1.0, double 1.0, double 0.0>
  %v1 = fsub fast <4 x double> %x, <double 2.0, double 1.0, double 1.0, double 0.0>
  %v2 = fsub fast <4 x double> %x, <double 3.0, double 1.0, double 1.0, double 0.0>
  %v3 = fsub fast <4 x double> %x, <double 4.0, double 1.0, double 1.0, double 0.0>
  %g0a = shufflevector <4 x double> %v0, <4 x double> %v1, <4 x i32> <i32 0, i32 4, i32 undef, i32 undef>
  %g0b = shufflevector <4 x double> %g0a, <4 x double> %v2, <4 x i32> <i32 0, i32 1, i32 4, i32 undef>
  %g0c = shufflevector <4 x double> %g0b, <4 x double> %v3, <4 x i32> <i32 0, i32 1, i32 2, i32 4>
  %s0 = fmul fast <4 x double> %g0c, %g0c
  %g1a = shufflevector <4 x double> %v0, <4 x double> %v1, <4 x i32> <i32 1, i32 5, i32 undef, i32 undef>
  %g1b = shufflevector <4 x double> %g1a, <4 x double> %v2, <4 x i32> <i32 0, i32 1, i32 5, i32 undef>
  %g1c = shufflevector <4 x double> %g1b, <4 x double> %v3, <4 x i32> <i32 0, i32 1, i32 2, i32 5>
  %s1 = fmul fast <4 x double> %g1c, %g1c
  %g2a = shufflevector <4 x double> %v0, <4 x double> %v1, <4 x i32> <i32 2, i32 6, i32 undef, i32 undef>
  %g2b = shufflevector <4 x double> %g2a, <4 x double> %v2, <4 x i32> <i32 0, i32 1, i32 6, i32 undef>
  %g2c = shufflevector <4 x double> %g2b, <4 x double> %v3, <4 x i32> <i32 0, i32 1, i32 2, i32 6>
  %s2 = fmul fast <4 x double> %g2c, %g2c
  %t0 = fadd fast <4 x double> %s0, %s1
  %t1 = fadd fast <4 x double> %t0, %s2
  %d = fmul fast <4 x double> <double 0.0, double 0.0, double 0.0, double 0x7FF0000000000000>, %v0
  %x.next = fadd fast <4 x double> %x, %d
  %i.next = add i32 %i, 1
  %c = icmp slt i32 %i.next, 2
  br i1 %c, label %loop, label %exit

exit:
  %r = extractelement <4 x double> %t1, i32 0
  ret double %r
}
attributes #0 = { "target-features"="+avx" }
)""";

std::string reductions_replace(std::string ir, const std::string &from,
        const std::string &to) {
    size_t pos = 0;
    while ((pos = ir.find(from, pos)) != std::string::npos) {
        ir.replace(pos, from.size(), to);
        pos += to.size();
    }
    return ir;
}

#endif

} // namespace

TEST_CASE("LLVM horizontal reductions of four vector squares") {
    std::unique_ptr<llvm::TargetMachine> x86
        = reductions_target_machine("x86_64-unknown-linux-gnu", "x86-64");
    if (x86) {
#if LLVM_VERSION_MAJOR < 11
        // The pass needs LLVM 11 or later; before that it leaves the
        // function as it is.
        CHECK(!reductions_rewrite(reductions_sum_ir, *x86));
#else
        // The pass replaces the shuffle transpose with horizontal adds.
        CHECK(reductions_rewrite(reductions_sum_ir, *x86));
        // A CPU with AVX enables it without a target-features attribute,
        // which is how `--mcpu` and `--fast` describe the host CPU.
        CHECK(reductions_rewrite(reductions_replace(reductions_sum_ir,
            "\"target-features\"=\"+avx\"", "\"target-cpu\"=\"haswell\""),
            *x86));
        // Without AVX there is no horizontal add to build.
        CHECK(!reductions_rewrite(reductions_replace(reductions_sum_ir,
            "+avx", "+sse2"), *x86));
        CHECK(!reductions_rewrite(reductions_replace(reductions_sum_ir,
            "\"target-features\"=\"+avx\"", "\"target-cpu\"=\"x86-64\""),
            *x86));
        // Without fast math the shuffle chain is not rewritten.
        CHECK(!reductions_rewrite(reductions_replace(reductions_sum_ir,
            " fast ", " "), *x86));
        // A term added twice is not a tree. Rewriting it as one sum would
        // drop the repeated factor.
        CHECK(!reductions_rewrite(reductions_diamond_ir, *x86));
        // A fourth component that is zero on entry stays zero when the
        // update multiplies it by itself or by a finite constant. A loaded
        // value may be an infinity there, as may a constant one, and 0*Inf
        // is NaN.
        const std::string differences =
            "  %v0 = fsub fast <4 x double> %x, <double 1.0, double 1.0, double 1.0, double 0.0>\n"
            "  %v1 = fsub fast <4 x double> %x, <double 2.0, double 1.0, double 1.0, double 0.0>\n"
            "  %v2 = fsub fast <4 x double> %x, <double 3.0, double 1.0, double 1.0, double 0.0>\n"
            "  %v3 = fsub fast <4 x double> %x, <double 4.0, double 1.0, double 1.0, double 0.0>\n";
        CHECK(!reductions_loop_masks(differences,
            "  %d = fmul fast <4 x double> %v0, %v0\n"
            "  %x.next = fadd fast <4 x double> %x, %d\n", *x86));
        CHECK(reductions_loop_masks(differences,
            "  %d = fmul fast <4 x double> %la, %v0\n"
            "  %x.next = fadd fast <4 x double> %x, %d\n", *x86));
        CHECK(!reductions_loop_masks(differences,
            "  %d = fmul fast <4 x double> %v0, <double 1.0, double 1.0, double 1.0, double 2.0>\n"
            "  %x.next = fadd fast <4 x double> %x, %d\n", *x86));
        CHECK(reductions_loop_masks(differences,
            "  %d = fmul fast <4 x double> <double 1.0, double 1.0, double 1.0, double 0.0>, %la\n"
            "  %x.next = fadd fast <4 x double> %x, %d\n", *x86));
        CHECK(reductions_loop_masks(differences,
            "  %d = fmul fast <4 x double> <double 0.0, double 0.0, double 0.0, double 0x7FF0000000000000>, %v0\n"
            "  %x.next = fadd fast <4 x double> %x, %d\n", *x86));
        // Adding a loaded vector can make it nonzero after the first
        // iteration.
        CHECK(reductions_loop_masks(differences,
            "  %x.next = fadd fast <4 x double> %x, %la\n", *x86));
        // A product of two vectors whose fourth components are unknown.
        CHECK(reductions_loop_masks(
            "  %v0 = fmul fast <4 x double> %la, %lb\n"
            "  %v1 = fmul fast <4 x double> %lb, %la\n"
            "  %v2 = fmul fast <4 x double> %la, %la\n"
            "  %v3 = fmul fast <4 x double> %lb, %lb\n",
            "  %x.next = fadd fast <4 x double> %x, %x\n", *x86));
        // A shuffle that moves a nonzero component into the fourth one.
        // The rewrite reads components 0 to 2 through it, so it rebuilds
        // the vectors from %x, whose fourth component stays zero.
        CHECK(!reductions_loop_masks(
            "  %v0 = shufflevector <4 x double> %x, <4 x double> <double 1.0, double 1.0, double 1.0, double 0.0>, <4 x i32> <i32 0, i32 1, i32 2, i32 4>\n"
            "  %v1 = fsub fast <4 x double> %x, %v0\n"
            "  %v2 = fsub fast <4 x double> %v0, %x\n"
            "  %v3 = fadd fast <4 x double> %v0, %x\n",
            "  %x.next = fadd fast <4 x double> %x, %x\n", *x86));
#endif
    }
    // Other targets are left to LLVM.
    std::unique_ptr<llvm::TargetMachine> arm
        = reductions_target_machine("aarch64-apple-macosx", "generic");
    if (arm) {
        CHECK(!reductions_rewrite(reductions_sum_ir, *arm));
    }
    // The rewritten function computes the same value when it runs. It
    // needs AVX to run, so it only runs on an x86 host with AVX.
#if (defined(__x86_64__) || defined(__i386__)) && defined(__GNUC__) \
    && LLVM_VERSION_MAJOR >= 11
    if (x86 && __builtin_cpu_supports("avx")) {
        LCompilers::LLVMEvaluator e;
        std::unique_ptr<llvm::Module> m = e.parse_module(reductions_sum_ir, "");
        CHECK(LCompilers::recombine_horizontal_reductions(
            *m->getFunction("reductions_sum"), *x86));
        e.add_module(std::move(m));
        CHECK(e.execfn<double>("reductions_sum") == 14.0);

        LCompilers::LLVMEvaluator lanes;
        std::unique_ptr<llvm::Module> lanes_m
            = lanes.parse_module(reductions_lanes_ir, "");
        llvm::Function &lanes_f = *lanes_m->getFunction("reductions_lanes");
        CHECK(LCompilers::recombine_horizontal_reductions(lanes_f, *x86));
        bool masked = false;
        for (llvm::BasicBlock &bb : lanes_f) {
            for (llvm::Instruction &i : bb) {
                masked = masked || clears_lane3(i);
            }
        }
        CHECK(masked);
        lanes.add_module(std::move(lanes_m));
        // 1*(1+4+9) + 2*(4+9+16) + 4*(9+16+25) + 8*(16+25+36)
        CHECK(lanes.execfn<double>("reductions_lanes") == 888.0);

        LCompilers::LLVMEvaluator diamond;
        std::unique_ptr<llvm::Module> diamond_m
            = diamond.parse_module(reductions_diamond_ir, "");
        CHECK(!LCompilers::recombine_horizontal_reductions(
            *diamond_m->getFunction("reductions_sum"), *x86));
        diamond.add_module(std::move(diamond_m));
        CHECK(diamond.execfn<double>("reductions_sum") == 15.0);

        // 0*Inf in the unused lane must not reach the horizontal add.
        LCompilers::LLVMEvaluator inf;
        std::unique_ptr<llvm::Module> inf_m
            = inf.parse_module(reductions_inf_ir, "");
        llvm::Function &inf_f = *inf_m->getFunction("reductions_inf");
        CHECK(LCompilers::recombine_horizontal_reductions(inf_f, *x86));
        bool inf_cleared = false;
        for (llvm::BasicBlock &bb : inf_f) {
            for (llvm::Instruction &i : bb) {
                inf_cleared = inf_cleared || clears_lane3(i);
            }
        }
        CHECK(inf_cleared);
        inf.add_module(std::move(inf_m));
        CHECK(inf.execfn<double>("reductions_inf") == 5.0);
    }
#endif
}

#if LLVM_VERSION_MAJOR >= 11
// Sums of the squares of components 0 to 2 of a-b, a-c, b-c, b-d, c-a and
// d-b, in the lane layouts of newer SLP vectorizers. Each function returns
// the lanes of its sum weighted by 1, 2, 4, ... The squared lengths are
// |a-b|^2 = 6, |a-c|^2 = |c-a|^2 = 11, |b-c|^2 = 17, |b-d|^2 = |d-b|^2 = 24.
const std::string reductions_layouts_ir = R"""(
; Two lanes: component 1 is gathered first and subtracted after, the others
; are gathered from the differences. 6 + 2*11 = 28.
define double @reductions_two() #0 {
entry:
  %a = freeze <4 x double> <double 1.0, double 2.0, double 3.0, double 0.0>
  %b = freeze <4 x double> <double 2.0, double 3.0, double 5.0, double 0.0>
  %c = freeze <4 x double> <double 4.0, double 1.0, double 2.0, double 0.0>
  %a1 = shufflevector <4 x double> %a, <4 x double> %a, <2 x i32> <i32 1, i32 1>
  %bc1 = shufflevector <4 x double> %b, <4 x double> %c, <2 x i32> <i32 1, i32 5>
  %e1 = fsub fast <2 x double> %a1, %bc1
  %s1 = fmul fast <2 x double> %e1, %e1
  %ab = fsub fast <4 x double> %a, %b
  %ac = fsub fast <4 x double> %a, %c
  %e0 = shufflevector <4 x double> %ab, <4 x double> %ac, <2 x i32> <i32 0, i32 4>
  %s0 = fmul fast <2 x double> %e0, %e0
  %p = fadd fast <2 x double> %s1, %s0
  %e2 = shufflevector <4 x double> %ab, <4 x double> %ac, <2 x i32> <i32 2, i32 6>
  %s2 = fmul fast <2 x double> %e2, %e2
  %sum = fadd fast <2 x double> %p, %s2
  %l0 = extractelement <2 x double> %sum, i32 0
  %l1 = extractelement <2 x double> %sum, i32 1
  %w1 = fmul double %l1, 2.0
  %r = fadd double %l0, %w1
  ret double %r
}

; Four lanes: two-lane partial sums of components 0 and 1, concatenated, plus
; the squares of component 2. 6 + 2*11 + 4*17 + 8*24 = 288.
define double @reductions_four() #0 {
entry:
  %a = freeze <4 x double> <double 1.0, double 2.0, double 3.0, double 0.0>
  %b = freeze <4 x double> <double 2.0, double 3.0, double 5.0, double 0.0>
  %c = freeze <4 x double> <double 4.0, double 1.0, double 2.0, double 0.0>
  %d = freeze <4 x double> <double 0.0, double -1.0, double 7.0, double 0.0>
  %ab = fsub fast <4 x double> %a, %b
  %ac = fsub fast <4 x double> %a, %c
  %bc = fsub fast <4 x double> %b, %c
  %bd = fsub fast <4 x double> %b, %d
  %e0 = shufflevector <4 x double> %ab, <4 x double> %ac, <2 x i32> <i32 0, i32 4>
  %s0 = fmul fast <2 x double> %e0, %e0
  %e1 = shufflevector <4 x double> %ab, <4 x double> %ac, <2 x i32> <i32 1, i32 5>
  %s1 = fmul fast <2 x double> %e1, %e1
  %p = fadd fast <2 x double> %s0, %s1
  %f0 = shufflevector <4 x double> %bc, <4 x double> %bd, <2 x i32> <i32 0, i32 4>
  %t0 = fmul fast <2 x double> %f0, %f0
  %f1 = shufflevector <4 x double> %bc, <4 x double> %bd, <2 x i32> <i32 1, i32 5>
  %t1 = fmul fast <2 x double> %f1, %f1
  %q = fadd fast <2 x double> %t0, %t1
  %pq = shufflevector <2 x double> %p, <2 x double> %q, <4 x i32> <i32 0, i32 1, i32 2, i32 3>
  %e2 = shufflevector <4 x double> %ab, <4 x double> %ac, <2 x i32> <i32 2, i32 6>
  %f2 = shufflevector <4 x double> %bc, <4 x double> %bd, <2 x i32> <i32 2, i32 6>
  %g2 = shufflevector <2 x double> %e2, <2 x double> %f2, <4 x i32> <i32 0, i32 1, i32 2, i32 3>
  %u2 = fmul fast <4 x double> %g2, %g2
  %sum = fadd fast <4 x double> %pq, %u2
  %w = fmul <4 x double> %sum, <double 1.0, double 2.0, double 4.0, double 8.0>
  %l0 = extractelement <4 x double> %w, i32 0
  %l1 = extractelement <4 x double> %w, i32 1
  %l2 = extractelement <4 x double> %w, i32 2
  %l3 = extractelement <4 x double> %w, i32 3
  %h0 = fadd double %l0, %l1
  %h1 = fadd double %l2, %l3
  %r = fadd double %h0, %h1
  ret double %r
}

; Eight lanes over vectors that hold two four element vectors each: a, b in
; %ab and c, d in %cd. Lanes 4 to 7 repeat lanes 0 to 3, which are a-c, b-d,
; c-a and d-b. 11*(1+4+16+64) + 24*(2+8+32+128) = 5015.
define double @reductions_eight() #0 {
entry:
  %ab = freeze <8 x double> <double 1.0, double 2.0, double 3.0, double 0.0, double 2.0, double 3.0, double 5.0, double 0.0>
  %cd = freeze <8 x double> <double 4.0, double 1.0, double 2.0, double 0.0, double 0.0, double -1.0, double 7.0, double 0.0>
  %x = fsub fast <8 x double> %ab, %cd
  %y = fsub fast <8 x double> %cd, %ab
  %g0 = shufflevector <8 x double> %x, <8 x double> %y, <8 x i32> <i32 0, i32 4, i32 8, i32 12, i32 0, i32 4, i32 8, i32 12>
  %s0 = fmul fast <8 x double> %g0, %g0
  %g1 = shufflevector <8 x double> %x, <8 x double> %y, <8 x i32> <i32 1, i32 5, i32 9, i32 13, i32 1, i32 5, i32 9, i32 13>
  %s1 = fmul fast <8 x double> %g1, %g1
  %g2 = shufflevector <8 x double> %x, <8 x double> %y, <8 x i32> <i32 2, i32 6, i32 10, i32 14, i32 2, i32 6, i32 10, i32 14>
  %s2 = fmul fast <8 x double> %g2, %g2
  %t = fadd fast <8 x double> %s0, %s1
  %sum = fadd fast <8 x double> %t, %s2
  %w = fmul <8 x double> %sum, <double 1.0, double 2.0, double 4.0, double 8.0, double 16.0, double 32.0, double 64.0, double 128.0>
  %lo = shufflevector <8 x double> %w, <8 x double> %w, <4 x i32> <i32 0, i32 1, i32 2, i32 3>
  %hi = shufflevector <8 x double> %w, <8 x double> %w, <4 x i32> <i32 4, i32 5, i32 6, i32 7>
  %v = fadd <4 x double> %lo, %hi
  %l0 = extractelement <4 x double> %v, i32 0
  %l1 = extractelement <4 x double> %v, i32 1
  %l2 = extractelement <4 x double> %v, i32 2
  %l3 = extractelement <4 x double> %v, i32 3
  %h0 = fadd double %l0, %l1
  %h1 = fadd double %l2, %l3
  %r = fadd double %h0, %h1
  ret double %r
}
attributes #0 = { "target-features"="+avx" }
)""";

TEST_CASE("LLVM horizontal reductions in the lane layouts of newer SLP") {
    // SLP of LLVM 23 sums squares in two, four and eight lanes, concatenates
    // partial sums, and subtracts before or after gathering a component.
    // The pass follows every lane to the same vector expression.
    std::unique_ptr<llvm::TargetMachine> x86
        = reductions_target_machine("x86_64-unknown-linux-gnu", "x86-64");
    if (!x86) {
        return;
    }
    const std::pair<const char *, double> functions[] = {
        {"reductions_two", 28.0},
        {"reductions_four", 288.0},
        {"reductions_eight", 5015.0},
    };
    for (const auto &function : functions) {
        INFO(function.first);
        LCompilers::LLVMEvaluator e;
        std::unique_ptr<llvm::Module> m
            = e.parse_module(reductions_layouts_ir, "");
        llvm::Function &f = *m->getFunction(function.first);
        CHECK(LCompilers::may_recombine_horizontal_reductions(f, *x86));
        CHECK(LCompilers::recombine_horizontal_reductions(f, *x86));
        std::string err;
        llvm::raw_string_ostream os(err);
        CHECK(!llvm::verifyFunction(f, &os));
        CHECK(calls_intrinsic_like(f, "hadd"));
#if (defined(__x86_64__) || defined(__i386__)) && defined(__GNUC__)
        if (__builtin_cpu_supports("avx")) {
            e.add_module(std::move(m));
            CHECK(e.execfn<double>(function.first) == function.second);
        }
#endif
    }
}
#endif

#if LLVM_VERSION_MAJOR >= 11
// Lanes 0 and 1 of the sum are |(a-b)*c|^2 = 6 and |(a-d)*c|^2 = 11 over
// components 0 to 2; the function returns 6 + 2*11 = 28. The original code
// never computes component 3 of a-b, which is Inf - Inf. Component 3 of c is
// zero, but the rewrite rebuilds (a-b)*c on whole vectors, where it is NaN,
// so it must clear lane 3 of the square.
const std::string reductions_rebuilt_ir = R"""(
define double @reductions_rebuilt() #0 {
entry:
  %a = freeze <4 x double> <double 1.0, double 2.0, double 3.0, double 0x7FF0000000000000>
  %b = freeze <4 x double> <double 2.0, double 3.0, double 5.0, double 0x7FF0000000000000>
  %d = freeze <4 x double> <double 4.0, double 1.0, double 2.0, double 0.0>
  %w = freeze <4 x double> <double 1.0, double 1.0, double 1.0, double 1.0>
  %c = fmul fast <4 x double> %w, <double 1.0, double 1.0, double 1.0, double 0.0>
  %a0 = shufflevector <4 x double> %a, <4 x double> undef, <2 x i32> <i32 0, i32 0>
  %bd0 = shufflevector <4 x double> %b, <4 x double> %d, <2 x i32> <i32 0, i32 4>
  %c0 = shufflevector <4 x double> %c, <4 x double> undef, <2 x i32> <i32 0, i32 0>
  %e0 = fsub fast <2 x double> %a0, %bd0
  %f0 = fmul fast <2 x double> %e0, %c0
  %s0 = fmul fast <2 x double> %f0, %f0
  %a1 = shufflevector <4 x double> %a, <4 x double> undef, <2 x i32> <i32 1, i32 1>
  %bd1 = shufflevector <4 x double> %b, <4 x double> %d, <2 x i32> <i32 1, i32 5>
  %c1 = shufflevector <4 x double> %c, <4 x double> undef, <2 x i32> <i32 1, i32 1>
  %e1 = fsub fast <2 x double> %a1, %bd1
  %f1 = fmul fast <2 x double> %e1, %c1
  %s1 = fmul fast <2 x double> %f1, %f1
  %a2 = shufflevector <4 x double> %a, <4 x double> undef, <2 x i32> <i32 2, i32 2>
  %bd2 = shufflevector <4 x double> %b, <4 x double> %d, <2 x i32> <i32 2, i32 6>
  %c2 = shufflevector <4 x double> %c, <4 x double> undef, <2 x i32> <i32 2, i32 2>
  %e2 = fsub fast <2 x double> %a2, %bd2
  %f2 = fmul fast <2 x double> %e2, %c2
  %s2 = fmul fast <2 x double> %f2, %f2
  %p = fadd fast <2 x double> %s0, %s1
  %sum = fadd fast <2 x double> %p, %s2
  %l0 = extractelement <2 x double> %sum, i32 0
  %l1 = extractelement <2 x double> %sum, i32 1
  %w1 = fmul double %l1, 2.0
  %r = fadd double %l0, %w1
  ret double %r
}
attributes #0 = { "target-features"="+avx" }
)""";

TEST_CASE("LLVM horizontal reductions clear lane 3 of rebuilt products") {
    // A zero factor makes a fast product zero only where the original code
    // computes the product. The rewrite computes component 3 of every
    // vector it rebuilds, so it clears that lane.
    std::unique_ptr<llvm::TargetMachine> x86
        = reductions_target_machine("x86_64-unknown-linux-gnu", "x86-64");
    if (!x86) {
        return;
    }
    LCompilers::LLVMEvaluator e;
    std::unique_ptr<llvm::Module> m = e.parse_module(reductions_rebuilt_ir, "");
    llvm::Function &f = *m->getFunction("reductions_rebuilt");
    CHECK(LCompilers::recombine_horizontal_reductions(f, *x86));
    std::string err;
    llvm::raw_string_ostream os(err);
    CHECK(!llvm::verifyFunction(f, &os));
    CHECK(calls_intrinsic_like(f, "hadd"));
    bool masked = false;
    for (llvm::BasicBlock &bb : f) {
        for (llvm::Instruction &i : bb) {
            masked = masked || clears_lane3(i);
        }
    }
    CHECK(masked);
#if (defined(__x86_64__) || defined(__i386__)) && defined(__GNUC__)
    if (__builtin_cpu_supports("avx")) {
        e.add_module(std::move(m));
        CHECK(e.execfn<double>("reductions_rebuilt") == 28.0);
    }
#endif
}
#endif

#if LLVM_VERSION_MAJOR >= 11
// The squared vector is a select of a fast product a*w and of w, where
// component 3 of w is zero. Lane 0 of the sum is 1^2+2^2+3^2 = 14 for
// a = (1, 2, 3, Inf) and w = 1. The original code never reads component 3 of
// a*w, which is Inf*0 = NaN, so the rewrite must clear lane 3 of the square.
const std::string reductions_anyleaf_ir = R"""(
define double @reductions_anyleaf(<4 x double> %a, <4 x double> %w0, i1 %c) #0 {
entry:
  %w = insertelement <4 x double> %w0, double 0.0, i32 3
  %m = fmul fast <4 x double> %a, %w
  %v = select i1 %c, <4 x double> %m, <4 x double> %w
  %g0a = shufflevector <4 x double> %v, <4 x double> %v, <4 x i32> <i32 0, i32 4, i32 undef, i32 undef>
  %g0b = shufflevector <4 x double> %g0a, <4 x double> %v, <4 x i32> <i32 0, i32 1, i32 4, i32 undef>
  %g0c = shufflevector <4 x double> %g0b, <4 x double> %v, <4 x i32> <i32 0, i32 1, i32 2, i32 4>
  %s0 = fmul fast <4 x double> %g0c, %g0c
  %g1a = shufflevector <4 x double> %v, <4 x double> %v, <4 x i32> <i32 1, i32 5, i32 undef, i32 undef>
  %g1b = shufflevector <4 x double> %g1a, <4 x double> %v, <4 x i32> <i32 0, i32 1, i32 5, i32 undef>
  %g1c = shufflevector <4 x double> %g1b, <4 x double> %v, <4 x i32> <i32 0, i32 1, i32 2, i32 5>
  %s1 = fmul fast <4 x double> %g1c, %g1c
  %g2a = shufflevector <4 x double> %v, <4 x double> %v, <4 x i32> <i32 2, i32 6, i32 undef, i32 undef>
  %g2b = shufflevector <4 x double> %g2a, <4 x double> %v, <4 x i32> <i32 0, i32 1, i32 6, i32 undef>
  %g2c = shufflevector <4 x double> %g2b, <4 x double> %v, <4 x i32> <i32 0, i32 1, i32 2, i32 6>
  %s2 = fmul fast <4 x double> %g2c, %g2c
  %t0 = fadd fast <4 x double> %s0, %s1
  %t1 = fadd fast <4 x double> %t0, %s2
  %l0 = extractelement <4 x double> %t1, i32 0
  ret double %l0
}

define double @reductions_anyleaf_main() #0 {
  %r = call double @reductions_anyleaf(<4 x double> <double 1.0, double 2.0, double 3.0, double 0x7FF0000000000000>, <4 x double> <double 1.0, double 1.0, double 1.0, double 1.0>, i1 true)
  ret double %r
}
attributes #0 = { "target-features"="+avx" }
)""";

TEST_CASE("LLVM horizontal reductions clear lane 3 of products of the original code") {
    // A zero factor does not make a product zero where the other factor
    // may be an infinity, even if the original code computes the product:
    // it never reads that lane, and vhaddpd does.
    std::unique_ptr<llvm::TargetMachine> x86
        = reductions_target_machine("x86_64-unknown-linux-gnu", "x86-64");
    if (!x86) {
        return;
    }
    LCompilers::LLVMEvaluator e;
    std::unique_ptr<llvm::Module> m = e.parse_module(reductions_anyleaf_ir, "");
    llvm::Function &f = *m->getFunction("reductions_anyleaf");
    CHECK(LCompilers::recombine_horizontal_reductions(f, *x86));
    std::string err;
    llvm::raw_string_ostream os(err);
    CHECK(!llvm::verifyFunction(f, &os));
    bool masked = false;
    for (llvm::BasicBlock &bb : f) {
        for (llvm::Instruction &i : bb) {
            masked = masked || clears_lane3(i);
        }
    }
    CHECK(masked);
#if (defined(__x86_64__) || defined(__i386__)) && defined(__GNUC__)
    if (__builtin_cpu_supports("avx")) {
        e.add_module(std::move(m));
        CHECK(e.execfn<double>("reductions_anyleaf_main") == 14.0);
    }
#endif
}
#endif

#if LLVM_VERSION_MAJOR >= 11
// Constant element indices past the end of a vector, whose result is
// poison. In @reductions_oob a squared element is read through one, and in
// @reductions_oob_zero the lane 3 proof follows one.
const std::string reductions_oob_ir = R"""(
define <4 x double> @reductions_oob(<4 x double> %a, <4 x double> %b) #0 {
entry:
  %s = shufflevector <4 x double> %a, <4 x double> %b, <4 x i32> <i32 0, i32 1, i32 2, i32 3>
  %e = extractelement <4 x double> %s, i32 7
  %v = insertelement <4 x double> %a, double %e, i32 0
  %q = fmul fast <4 x double> %v, %v
  %q1 = fmul fast <4 x double> %b, %b
  %q2 = fmul fast <4 x double> %a, %a
  %t = fadd fast <4 x double> %q, %q1
  %r = fadd fast <4 x double> %t, %q2
  ret <4 x double> %r
}

define <4 x double> @reductions_oob_zero(<4 x double> %a, <4 x double> %b, i1 %c) #0 {
entry:
  %s = shufflevector <4 x double> %a, <4 x double> %b, <4 x i32> <i32 0, i32 1, i32 2, i32 3>
  %e = extractelement <4 x double> %s, i32 7
  %v = insertelement <4 x double> %a, double %e, i32 3
  %v0 = select i1 %c, <4 x double> %v, <4 x double> %a
  %g0a = shufflevector <4 x double> %v0, <4 x double> %b, <4 x i32> <i32 0, i32 4, i32 undef, i32 undef>
  %g0b = shufflevector <4 x double> %g0a, <4 x double> %a, <4 x i32> <i32 0, i32 1, i32 4, i32 undef>
  %g0c = shufflevector <4 x double> %g0b, <4 x double> %b, <4 x i32> <i32 0, i32 1, i32 2, i32 4>
  %s0 = fmul fast <4 x double> %g0c, %g0c
  %g1a = shufflevector <4 x double> %v0, <4 x double> %b, <4 x i32> <i32 1, i32 5, i32 undef, i32 undef>
  %g1b = shufflevector <4 x double> %g1a, <4 x double> %a, <4 x i32> <i32 0, i32 1, i32 5, i32 undef>
  %g1c = shufflevector <4 x double> %g1b, <4 x double> %b, <4 x i32> <i32 0, i32 1, i32 2, i32 5>
  %s1 = fmul fast <4 x double> %g1c, %g1c
  %g2a = shufflevector <4 x double> %v0, <4 x double> %b, <4 x i32> <i32 2, i32 6, i32 undef, i32 undef>
  %g2b = shufflevector <4 x double> %g2a, <4 x double> %a, <4 x i32> <i32 0, i32 1, i32 6, i32 undef>
  %g2c = shufflevector <4 x double> %g2b, <4 x double> %b, <4 x i32> <i32 0, i32 1, i32 2, i32 6>
  %s2 = fmul fast <4 x double> %g2c, %g2c
  %t0 = fadd fast <4 x double> %s0, %s1
  %t1 = fadd fast <4 x double> %t0, %s2
  ret <4 x double> %t1
}
attributes #0 = { "target-features"="+avx" }
)""";

TEST_CASE("LLVM horizontal reductions through out of range element indices") {
    // An element read through an index past the end is not a component,
    // and it is not known to be zero.
    std::unique_ptr<llvm::TargetMachine> x86
        = reductions_target_machine("x86_64-unknown-linux-gnu", "x86-64");
    if (!x86) {
        return;
    }
    LCompilers::LLVMEvaluator e;
    std::unique_ptr<llvm::Module> m = e.parse_module(reductions_oob_ir, "");
    llvm::Function &oob = *m->getFunction("reductions_oob");
    CHECK(!LCompilers::recombine_horizontal_reductions(oob, *x86));
    CHECK(!calls_intrinsic_like(oob, "hadd"));
    llvm::Function &zero = *m->getFunction("reductions_oob_zero");
    CHECK(LCompilers::recombine_horizontal_reductions(zero, *x86));
    bool masked = false;
    for (llvm::BasicBlock &bb : zero) {
        for (llvm::Instruction &i : bb) {
            masked = masked || clears_lane3(i);
        }
    }
    CHECK(masked);
    std::string err;
    llvm::raw_string_ostream os(err);
    CHECK(!llvm::verifyModule(*m, &os));
}
#endif

#if LLVM_VERSION_MAJOR >= 11
TEST_CASE("LLVM horizontal reductions only where they may apply") {
    // The --fast pipeline runs its late cleanup and the rewrite only on the
    // functions `may_recombine_horizontal_reductions` accepts. Everywhere
    // else the function must stay exactly as it is.
    auto check = [](const std::string &ir, const llvm::TargetMachine &tm,
            bool applies) {
        LCompilers::LLVMEvaluator e;
        std::unique_ptr<llvm::Module> m = e.parse_module(ir, "");
        llvm::Function &f = *m->getFunction("reductions_sum");
        std::string before = e.module_to_string(*m);
        CHECK(LCompilers::may_recombine_horizontal_reductions(f, tm)
            == applies);
        CHECK(LCompilers::recombine_horizontal_reductions(f, tm) == applies);
        CHECK((e.module_to_string(*m) == before) == !applies);
    };
    // Fast vector arithmetic on AVX, but no sum of three squares.
    const std::string no_gather_ir = R"""(
define double @reductions_sum(<4 x double> %v0, <4 x double> %v1) #0 {
entry:
  %s0 = fmul fast <4 x double> %v0, %v0
  %s1 = fmul fast <4 x double> %v1, %v1
  %t0 = fadd fast <4 x double> %s0, %s1
  %t1 = fadd fast <4 x double> %t0, %v0
  %r = extractelement <4 x double> %t1, i32 0
  ret double %r
}
attributes #0 = { "target-features"="+avx" }
)""";
    std::unique_ptr<llvm::TargetMachine> x86
        = reductions_target_machine("x86_64-unknown-linux-gnu", "x86-64");
    if (x86) {
        // x86 with AVX, from target-features or from target-cpu.
        check(reductions_sum_ir, *x86, true);
        check(reductions_replace(reductions_sum_ir,
            "\"target-features\"=\"+avx\"", "\"target-cpu\"=\"haswell\""),
            *x86, true);
        // x86 without AVX.
        check(reductions_replace(reductions_sum_ir, "+avx", "+sse2"), *x86,
            false);
        check(reductions_replace(reductions_sum_ir,
            "\"target-features\"=\"+avx\"", "\"target-cpu\"=\"x86-64\""),
            *x86, false);
        // x86 with AVX, but nothing to rewrite.
        check(no_gather_ir, *x86, false);
        // A repeated term is not rewritten, and the late cleanup must not
        // run for it either.
        check(reductions_diamond_ir, *x86, false);
    }
    // AArch64, such as Apple Silicon.
    std::unique_ptr<llvm::TargetMachine> arm
        = reductions_target_machine("arm64-apple-darwin", "generic");
    if (arm) {
        check(reductions_sum_ir, *arm, false);
    }
}

// The integration tests check the numeric result on every LLVM version.
// They stay green when the rewrite does not fire, so they do not notice a
// rewrite that stopped firing. LLVM 11's SLP loads each point of these
// routines as one four-wide vector, and this test requires the horizontal
// add there. LLVM 23 loads the points in two-element pieces, so no
// four-wide vector of a point exists to square, and the pass leaves the
// routines alone. "LLVM horizontal reductions in the lane layouts of newer
// SLP" covers the layouts of newer versions.
#ifdef HAVE_TARGET_X86
TEST_CASE("Fortran sum of squares becomes a horizontal add on Haswell") {
#if !defined(__x86_64__) && !defined(__i386__)
    return;
#else
    const std::string source = R"(module reductions_hadd_check
implicit none
integer, parameter :: np = 5, npair = 10
integer, parameter :: pair_i(npair) = [2, 3, 3, 4, 4, 4, 5, 5, 5, 5]
integer, parameter :: pair_j(npair) = [1, 1, 2, 1, 2, 3, 1, 2, 3, 4]
contains
subroutine distances(x, d2)
real(8), intent(in) :: x(4, np)
real(8), intent(out) :: d2(npair)
real(8) :: r(4, npair)
integer :: k, d
do k = 1, npair
    do d = 1, 4
        r(d, k) = x(d, pair_i(k)) - x(d, pair_j(k))
    end do
end do
do k = 1, npair
    d2(k) = r(1, k)*r(1, k) + r(2, k)*r(2, k) + r(3, k)*r(3, k)
end do
end subroutine
subroutine products(x, d2)
real(8), intent(in) :: x(4, np)
real(8), intent(out) :: d2(npair)
real(8) :: r(4, npair)
integer :: k, d
do k = 1, npair
    do d = 1, 4
        r(d, k) = x(d, pair_i(k)) * x(d, pair_j(k))
    end do
end do
do k = 1, npair
    d2(k) = r(1, k)*r(1, k) + r(2, k)*r(2, k) + r(3, k)*r(3, k)
end do
end subroutine
end module
)";
    Allocator al(64*1024);
    LCompilers::diag::Diagnostics diagnostics;
    CompilerOptions co;
    co.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
    co.platform = LCompilers::get_platform();
    co.po.fast = true;
    co.po.bounds_checking = false;
    co.po.strict_bounds_checking = false;
    co.mcpu = "haswell";
    LCompilers::LLVMEvaluator e(co);
    LCompilers::LFortran::AST::TranslationUnit_t* tu = TRY(
        LCompilers::LFortran::parse(al, source, diagnostics, co));
    LCompilers::LocationManager lm;
    LCompilers::LocationManager::FileLocations file;
    file.in_filename = "reductions_hadd_check.f90";
    lm.files.push_back(file);
    lm.file_ends.push_back(LCompilers::LFortran::source_offset(source.size()));
    LCompilers::ASR::TranslationUnit_t* asr = TRY(
        LCompilers::LFortran::ast_to_asr(al, *tu, diagnostics, nullptr, false,
            co, lm));
    LCompilers::PassManager lpm;
    lpm.use_default_passes();
    LCompilers::Result<std::unique_ptr<LCompilers::LLVMModule>> res
        = LCompilers::asr_to_llvm(*asr, diagnostics, e.get_context(),
            e.get_target_config(), al, lpm, co, "", "", "", lm);
    INFO(diagnostics.render2());
    REQUIRE(res.ok);
    e.opt(*res.result->m_m);
    auto requires_hadd = [&](const std::string &suffix) {
        llvm::Function *fn = nullptr;
        for (llvm::Function &f : *res.result->m_m) {
            std::string name = f.getName().str();
            if (!f.isDeclaration() && name.size() >= suffix.size()
                    && name.compare(name.size() - suffix.size(),
                        suffix.size(), suffix) == 0) {
                fn = &f;
            }
        }
        REQUIRE(fn != nullptr);
        INFO(fn->getName().str());
#if LLVM_VERSION_MAJOR == 11
        CHECK(calls_intrinsic_like(*fn, "hadd"));
#endif
    };
    requires_hadd("distances");
    requires_hadd("products");
#endif
}

// The tests below compile for the host, which must be x86.
#if defined(__x86_64__) || defined(__i386__)
namespace {

// The points of reductions_hadd_03 and reductions_hadd_04: five points that
// start from constants with a zero fourth component, held as contiguous four
// element vectors, and move under pairwise forces for a number of steps only
// known at run time. `force` sets w(k) from the squared distance d2.
std::string reductions_orbit_source(const std::string &force) {
    return R"(program reductions_orbit
implicit none
integer, parameter :: nb = 5, npair = 10
integer, parameter :: pair_i(npair) = [2,3,3,4,4,4,5,5,5,5]
integer, parameter :: pair_j(npair) = [1,1,2,1,2,3,1,2,3,4]
real(8) :: x(4,nb), v(4,nb)
call init(x, v)
call orbit(x, v, 100 + command_argument_count())
print *, x
contains
subroutine init(x, v)
    real(8), intent(out) :: x(4,nb), v(4,nb)
    real(8) :: p(4,nb)
    p(:,1) = [0d0, 0d0, 0d0, 0d0]
    p(:,2) = [1d0, 2d0, 3d0, 0d0]
    p(:,3) = [-2d0, 1d0, 4d0, 0d0]
    p(:,4) = [3d0, -1d0, 2d0, 0d0]
    p(:,5) = [5d0, 3d0, -2d0, 0d0]
    x = p
    v = 0
end subroutine
subroutine orbit(x, v, n)
    real(8), intent(inout) :: x(4,nb), v(4,nb)
    integer, intent(in) :: n
    real(8) :: r(4,npair), w(npair), d2
    real :: s
    integer :: step, i, k, d
    do step = 1, n
        do k = 1, npair
            do d = 1, 4
                r(d,k) = x(d,pair_i(k)) - x(d,pair_j(k))
            end do
        end do
        do k = 1, npair
            d2 = r(1,k)*r(1,k) + r(2,k)*r(2,k) + r(3,k)*r(3,k)
            )" + force + R"(
        end do
        do k = 1, npair
            do d = 1, 4
                v(d,pair_i(k)) = v(d,pair_i(k)) - r(d,k) * w(k)
                v(d,pair_j(k)) = v(d,pair_j(k)) + r(d,k) * w(k)
            end do
        end do
        do i = 1, nb
            do d = 1, 4
                x(d,i) = x(d,i) + 0.01d0 * v(d,i)
            end do
        end do
    end do
end subroutine
end program
)";
}

// Compiles `source` like `lfortran --fast --mcpu=haswell` and passes the
// optimized module to `check`.
void check_fast_haswell(const std::string &source,
        const std::function<void(llvm::Module &)> &check) {
    CompilerOptions co;
    co.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
    co.platform = LCompilers::get_platform();
    co.po.fast = true;
    co.po.bounds_checking = false;
    co.po.strict_bounds_checking = false;
    co.mcpu = "haswell";
    LCompilers::FortranEvaluator e(co);
    LCompilers::LocationManager lm;
    LCompilers::LocationManager::FileLocations file;
    file.in_filename = "reductions_orbit.f90";
    lm.files.push_back(file);
    lm.file_ends.push_back(LCompilers::LFortran::source_offset(source.size()));
    LCompilers::diag::Diagnostics diagnostics;
    LCompilers::PassManager lpm;
    lpm.skip_llvm_backend_passes();
    lpm.use_default_passes();
    LCompilers::Result<std::unique_ptr<LCompilers::LLVMModule>> res
        = e.get_llvm2(source, lm, lpm, diagnostics);
    INFO(diagnostics.render2());
    REQUIRE(res.ok);
    check(*res.result->m_m);
}

// A floating point constant or a splat of one, equal to `value`.
bool is_fp_constant(llvm::Value *v, double value) {
    auto *c = llvm::dyn_cast<llvm::Constant>(v);
    if (c && c->getType()->isVectorTy()) {
        c = c->getSplatValue();
    }
    auto *fp = llvm::dyn_cast_or_null<llvm::ConstantFP>(c);
    return fp && fp->isExactlyValue(value);
}

// The Newton step `e * (x*e*e - 3)` of the estimate `e = 1/sqrt(x)`: a sum
// with -3 among the first users of `e`.
bool has_newton_step(llvm::Instruction *estimate) {
    std::vector<llvm::Instruction *> level{estimate};
    for (int depth = 0; depth < 3; depth++) {
        std::vector<llvm::Instruction *> next;
        for (llvm::Instruction *i : level) {
            for (llvm::User *u : i->users()) {
                auto *ui = llvm::dyn_cast<llvm::Instruction>(u);
                if (!ui) {
                    continue;
                }
                if (ui->getOpcode() == llvm::Instruction::FAdd
                        && is_fp_constant(ui->getOperand(1), -3)) {
                    return true;
                }
                next.push_back(ui);
            }
        }
        level = next;
    }
    return false;
}

} // namespace

TEST_CASE("Fortran single precision 1/sqrt keeps its Newton step on Haswell") {
    // The code generator emits the bare 12 bit estimate for every single
    // precision 1/sqrt of a function with the attribute
    // `reciprocal-estimates`, so each must be refined by the Newton step that
    // expand_reciprocal_sqrt writes in IR. The step must survive the IR
    // optimizations that follow the horizontal reduction rewrite.
    check_fast_haswell(reductions_orbit_source(
            "s = 1 / sqrt(real(d2)); w(k) = 0.01d0 * s**3"),
            [](llvm::Module &m) {
        int estimates = 0;
        for (llvm::Function &f : m) {
            if (f.isDeclaration()
                    || !f.hasFnAttribute("reciprocal-estimates")) {
                continue;
            }
            INFO(f.getName().str());
            for (llvm::BasicBlock &bb : f) {
                for (llvm::Instruction &i : bb) {
                    auto *call = llvm::dyn_cast<llvm::CallInst>(&i);
                    llvm::Function *callee
                        = call ? call->getCalledFunction() : nullptr;
                    if (!callee
                            || callee->getName().str().rfind("llvm.sqrt", 0)
                                != 0
                            || !call->getType()->getScalarType()->isFloatTy()) {
                        continue;
                    }
                    for (llvm::User *u : call->users()) {
                        auto *div = llvm::dyn_cast<llvm::Instruction>(u);
                        if (div && div->getOpcode() == llvm::Instruction::FDiv
                                && div->getOperand(1) == call) {
                            estimates++;
                            CHECK(is_fp_constant(div->getOperand(0), 1));
                            CHECK(has_newton_step(div));
                        }
                    }
                }
            }
        }
        // The pass expands the 1/sqrt of the force loop.
        CHECK(estimates > 0);
    });
}

TEST_CASE("Fortran pairwise distances become horizontal adds on Haswell") {
    // reductions_hadd_03. After inlining, the SLP vectorizer of LLVM 23
    // builds the squared pair distances in a lane layout of newer SLP, which
    // the horizontal reduction pass must still recognize. LLVM 11 vectorizes
    // this program differently and the pass leaves it alone.
    check_fast_haswell(reductions_orbit_source(
            "w(k) = 0.01d0 / (d2 * sqrt(d2))"),
            [](llvm::Module &m) {
        llvm::Function *main_fn = m.getFunction("main");
        REQUIRE(main_fn != nullptr);
#if LLVM_VERSION_MAJOR >= 23
        CHECK(calls_intrinsic_like(*main_fn, "hadd"));
#endif
    });
}
#endif
#endif // HAVE_TARGET_X86
#endif

#if defined(__x86_64__)
TEST_CASE("LLVM horizontal reductions of loop-carried vectors set from constants") {
    // The positions start from constants whose fourth component is zero, and
    // the updates keep it zero. The loop passes leave loads of the initial
    // positions, which the late round of the --fast pipeline forwards, so the
    // horizontal reduction pass needs no mask in the time step loop.
    std::string source = R"(module reductions_late_gvn
implicit none
integer, parameter :: nb = 5, npair = 10
integer, parameter :: pair_i(npair) = [2,3,3,4,4,4,5,5,5,5]
integer, parameter :: pair_j(npair) = [1,1,2,1,2,3,1,2,3,4]
contains

subroutine init(x, v)
    real(8), intent(out) :: x(4,nb), v(4,nb)
    real(8) :: p(4,nb)
    p(:,1) = [0d0, 0d0, 0d0, 0d0]
    p(:,2) = [1d0, 2d0, 3d0, 0d0]
    p(:,3) = [-2d0, 1d0, 4d0, 0d0]
    p(:,4) = [3d0, -1d0, 2d0, 0d0]
    p(:,5) = [5d0, 3d0, -2d0, 0d0]
    x = p
    v = 0
end subroutine

real(8) function energy(x) result(e)
    real(8), intent(in) :: x(4,nb)
    real(8) :: r(4)
    integer :: k, d
    e = 0
    do k = 1, npair
        do d = 1, 4
            r(d) = x(d,pair_i(k)) - x(d,pair_j(k))
        end do
        e = e + 1 / sqrt(r(1)*r(1) + r(2)*r(2) + r(3)*r(3))
    end do
end function

real(8) function orbit(n) result(e)
    integer, intent(in) :: n
    real(8) :: x(4,nb), v(4,nb), r(4,npair), w(npair), d2
    integer :: step, i, k, d
    call init(x, v)
    e = energy(x)
    do step = 1, n
        do k = 1, npair
            do d = 1, 4
                r(d,k) = x(d,pair_i(k)) - x(d,pair_j(k))
            end do
        end do
        do k = 1, npair
            d2 = r(1,k)*r(1,k) + r(2,k)*r(2,k) + r(3,k)*r(3,k)
            w(k) = 0.01d0 / (d2 * sqrt(d2))
        end do
        do k = 1, npair
            do d = 1, 4
                v(d,pair_i(k)) = v(d,pair_i(k)) - r(d,k) * w(k)
                v(d,pair_j(k)) = v(d,pair_j(k)) + r(d,k) * w(k)
            end do
        end do
        do i = 1, nb
            do d = 1, 4
                x(d,i) = x(d,i) + 0.01d0 * v(d,i)
            end do
        end do
    end do
    e = e + energy(x)
end function

end module
)";
    Allocator al(64*1024);
    LCompilers::diag::Diagnostics diagnostics;
    CompilerOptions co;
    co.po.runtime_library_dir = LFORTRAN_BUILD_RUNTIME_DIR;
    co.platform = LCompilers::get_platform();
    co.po.fast = true;
    co.po.bounds_checking = false;
    co.po.strict_bounds_checking = false;
    co.mcpu = "haswell";
    LCompilers::LFortran::AST::TranslationUnit_t* tu = TRY(
        LCompilers::LFortran::parse(al, source, diagnostics, co));
    LCompilers::LocationManager lm;
    LCompilers::LocationManager::FileLocations file;
    file.in_filename = "reductions_late_gvn.f90";
    lm.files.push_back(file);
    lm.file_ends.push_back(LCompilers::LFortran::source_offset(source.size()));
    LCompilers::ASR::TranslationUnit_t* asr = TRY(
        LCompilers::LFortran::ast_to_asr(al, *tu, diagnostics, nullptr, false,
            co, lm));
    LCompilers::LLVMEvaluator e(co);
    LCompilers::PassManager lpm;
    lpm.use_default_passes();
    LCompilers::Result<std::unique_ptr<LCompilers::LLVMModule>>
        res = LCompilers::asr_to_llvm(*asr, diagnostics, e.get_context(),
            e.get_target_config(), al, lpm, co, "", "", "", lm);
    REQUIRE(res.ok);
    e.opt(*res.result->m_m);
    llvm::Function *orbit = nullptr;
    for (llvm::Function &f : *res.result->m_m) {
        std::string name = f.getName().str();
        if (!f.isDeclaration() && name.size() >= 5
                && name.compare(name.size() - 5, 5, "orbit") == 0) {
            orbit = &f;
        }
    }
    REQUIRE(orbit != nullptr);
    // The rewrite and clears_lane3 exist from LLVM 11. Earlier versions
    // still compile and optimize this module; they just do not have the pass.
#if LLVM_VERSION_MAJOR >= 11
    bool reduced = calls_intrinsic_like(*orbit, "hadd");
#if LLVM_VERSION_MAJOR == 11
    // The SLP vectorizer of LLVM 11 gathers the components of the pair
    // vectors with the transpose the pass matches. Other versions vectorize
    // this loop differently and the pass leaves it alone.
    CHECK(reduced);
#endif
    bool masked = false;
    for (llvm::BasicBlock &bb : *orbit) {
        for (llvm::Instruction &i : bb) {
            masked = masked || clears_lane3(i);
        }
    }
    if (reduced) {
        CHECK(!masked);
    }
#endif
}
#endif
#endif // __EMSCRIPTEN__
