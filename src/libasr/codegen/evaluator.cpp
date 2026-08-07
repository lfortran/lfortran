#include <algorithm>
#include <iostream>
#include <fstream>
#include <optional>

#include <llvm/IR/LLVMContext.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/Analysis/Passes.h>
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/ExecutionEngine/MCJIT.h>
#include <llvm/IR/Argument.h>
#include <llvm/IR/Attributes.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/ManagedStatic.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/IR/Verifier.h>
#include <llvm/MC/MCSubtargetInfo.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Scalar/InstSimplifyPass.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/IPO/AlwaysInliner.h>
#if LLVM_VERSION_MAJOR >= 9
#include <llvm/Transforms/Instrumentation/AddressSanitizer.h>
#include <llvm/Transforms/Instrumentation/ThreadSanitizer.h>
#endif
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/ExecutionEngine/ObjectCache.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Path.h>
#include <llvm/AsmParser/Parser.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Target/TargetOptions.h>
#if LLVM_VERSION_MAJOR >= 14
#    include <llvm/MC/TargetRegistry.h>
#else
#    include <llvm/Support/TargetRegistry.h>
#endif
#if LLVM_VERSION_MAJOR >= 17
    // TODO: removed from LLVM 17
    #include <llvm/Passes/PassBuilder.h>
#else
#    include <llvm/Transforms/IPO/PassManagerBuilder.h>
#endif

#if LLVM_VERSION_MAJOR >= 18
#    include <llvm/TargetParser/Host.h>
#    include <llvm/TargetParser/Triple.h>
#else
#    include <llvm/ADT/Triple.h>
#    include <llvm/Support/Host.h>
#endif
#if LLVM_VERSION_MAJOR < 18
#    include <llvm/Transforms/Vectorize.h>
#endif
#ifdef HAVE_TARGET_AARCH64
#    if LLVM_VERSION_MAJOR >= 18
#        include <llvm/TargetParser/AArch64TargetParser.h>
#    else
#        include <llvm/Support/AArch64TargetParser.h>
#    endif
#endif
#ifdef HAVE_TARGET_X86
#    if LLVM_VERSION_MAJOR >= 18
#        include <llvm/TargetParser/X86TargetParser.h>
#    else
#        include <llvm/Support/X86TargetParser.h>
#    endif
#endif

#include <libasr/codegen/KaleidoscopeJIT.h>
#include <libasr/codegen/evaluator.h>
#include <libasr/codegen/asr_to_llvm.h>
#include <libasr/codegen/asr_to_cpp.h>
#include <libasr/exception.h>
#include <libasr/asr.h>
#include <libasr/string_utils.h>

#ifdef HAVE_LFORTRAN_MLIR
#include <mlir/IR/BuiltinOps.h>
#include <mlir/Target/LLVMIR/Export.h>
#endif

// LLD wasm driver — included at global scope so LLD_HAS_DRIVER declares
// ::lld::wasm::link in the global lld namespace, not inside LCompilers.
#ifdef __EMSCRIPTEN__
#include <atomic>
#include <dlfcn.h>
#include <lld/Common/Driver.h>
LLD_HAS_DRIVER(wasm)
#endif

namespace LCompilers {

namespace {

void initialize_llvm_targets()
{
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();

#ifdef HAVE_TARGET_AARCH64
    LLVMInitializeAArch64Target();
    LLVMInitializeAArch64TargetInfo();
    LLVMInitializeAArch64TargetMC();
    LLVMInitializeAArch64AsmPrinter();
    LLVMInitializeAArch64AsmParser();
#endif
#ifdef HAVE_TARGET_X86
    LLVMInitializeX86Target();
    LLVMInitializeX86TargetInfo();
    LLVMInitializeX86TargetMC();
    LLVMInitializeX86AsmPrinter();
    LLVMInitializeX86AsmParser();
#endif
#ifdef HAVE_TARGET_WASM
    LLVMInitializeWebAssemblyTarget();
    LLVMInitializeWebAssemblyTargetInfo();
    LLVMInitializeWebAssemblyTargetMC();
    LLVMInitializeWebAssemblyAsmPrinter();
    LLVMInitializeWebAssemblyAsmParser();
#endif
}

const llvm::Target *get_llvm_target(const std::string &triple)
{
    std::string error;
#if LLVM_VERSION_MAJOR >= 21
    const llvm::Target *target = llvm::TargetRegistry::lookupTarget(
        llvm::Triple(triple), error);
#else
    const llvm::Target *target = llvm::TargetRegistry::lookupTarget(
        triple, error);
#endif
    if (!target) {
        throw LCompilersException(error);
    }
    return target;
}

bool is_host_target(const llvm::Triple &target)
{
    llvm::Triple host(llvm::Triple::normalize(
        llvm::sys::getDefaultTargetTriple()));
    return target.getArch() == host.getArch()
        && target.getOS() == host.getOS()
        && target.getEnvironment() == host.getEnvironment();
}

std::string join_features(const std::vector<std::string> &features)
{
    std::string result;
    for (size_t i = 0; i < features.size(); i++) {
        if (i > 0) {
            result += ",";
        }
        result += features[i];
    }
    return result;
}

std::vector<std::string> get_detected_host_features()
{
    std::vector<std::string> features;
#if LLVM_VERSION_MAJOR >= 22
    auto host_features = llvm::sys::getHostCPUFeatures();
#else
    llvm::StringMap<bool, llvm::MallocAllocator> host_features;
    llvm::sys::getHostCPUFeatures(host_features);
#endif
    features.reserve(host_features.size());
    for (const auto &feature: host_features) {
        features.push_back(
            std::string(feature.getValue() ? "+" : "-")
            + feature.getKey().str());
    }
    std::sort(features.begin(), features.end());
    return features;
}

#if defined(HAVE_TARGET_AARCH64) && LLVM_VERSION_MAJOR >= 18
std::vector<std::string> get_aarch64_extension_features(
    const llvm::AArch64::ExtensionSet &extensions)
{
#if LLVM_VERSION_MAJOR == 18
    std::vector<llvm::StringRef> parsed_features;
    extensions.toLLVMFeatureList(parsed_features);
    std::vector<std::string> features;
    features.reserve(parsed_features.size());
    for (llvm::StringRef feature: parsed_features) {
        features.push_back(feature.str());
    }
    return features;
#else
    std::vector<std::string> features;
    extensions.toLLVMFeatureList(features);
    return features;
#endif
}
#endif

std::vector<std::string> get_aarch64_cpu_features(
    const std::string &cpu, const llvm::Triple &triple)
{
#ifdef HAVE_TARGET_AARCH64
#if LLVM_VERSION_MAJOR >= 18
    std::optional<llvm::AArch64::CpuInfo> cpu_info
        = llvm::AArch64::parseCpu(cpu);
    if (!cpu_info) {
        throw LCompilersException("unsupported cpu '" + cpu
            + "' for target '" + triple.str() + "'");
    }
    llvm::AArch64::ExtensionSet extensions;
    extensions.addCPUDefaults(*cpu_info);
    return get_aarch64_extension_features(extensions);
#else
#if LLVM_VERSION_MAJOR >= 16
    const llvm::AArch64::CpuInfo &cpu_info
        = llvm::AArch64::parseCpu(cpu);
    if (cpu_info.Name == "invalid") {
        throw LCompilersException("unsupported cpu '" + cpu
            + "' for target '" + triple.str() + "'");
    }
    std::vector<llvm::StringRef> parsed_features = {
        cpu_info.Arch.ArchFeature
    };
    llvm::AArch64::getExtensionFeatures(
        llvm::AArch64::getDefaultExtensions(cpu, cpu_info.Arch),
        parsed_features);
#else
    llvm::AArch64::ArchKind arch = llvm::AArch64::parseCPUArch(cpu);
    if (arch == llvm::AArch64::ArchKind::INVALID) {
        throw LCompilersException("unsupported cpu '" + cpu
            + "' for target '" + triple.str() + "'");
    }
    std::vector<llvm::StringRef> parsed_features;
    llvm::AArch64::getArchFeatures(arch, parsed_features);
    llvm::AArch64::getExtensionFeatures(
        llvm::AArch64::getDefaultExtensions(cpu, arch), parsed_features);
#endif
    std::vector<std::string> features;
    features.reserve(parsed_features.size());
    for (llvm::StringRef feature: parsed_features) {
        features.push_back(feature.str());
    }
    return features;
#endif
#else
    (void)cpu;
    throw LCompilersException("cpu feature detection is not supported for target '"
        + triple.str() + "' by this LFortran build");
#endif
}

std::string get_host_cpu(const std::string &option,
    const LLVMTargetConfig &config)
{
    if (!config.host_target) {
        throw LCompilersException("`" + option
            + "=native` is only supported when targeting the host");
    }
    std::string cpu = llvm::sys::getHostCPUName().str();
    if (cpu.empty()) {
        throw LCompilersException("could not detect the host cpu for `"
            + option + "=native`");
    }
    return cpu;
}

void validate_cpu(const llvm::Target &target, const llvm::Triple &triple,
    const std::string &cpu, const std::string &option)
{
    if (cpu == "generic") {
        return;
    }
    std::unique_ptr<llvm::MCSubtargetInfo> subtarget(
#if LLVM_VERSION_MAJOR >= 21
        target.createMCSubtargetInfo(triple, "generic", "")
#else
        target.createMCSubtargetInfo(triple.str(), "generic", "")
#endif
    );
    if (!subtarget || !subtarget->isCPUStringValid(cpu)) {
        throw LCompilersException("unsupported cpu '" + cpu
            + "' for target '" + triple.str() + "' in `" + option + "`");
    }
}

std::vector<std::string> get_aarch64_arch_features(
    const std::string &march, const llvm::Triple &triple)
{
#ifdef HAVE_TARGET_AARCH64
    llvm::SmallVector<llvm::StringRef, 8> parts;
    llvm::StringRef(march).split(parts, '+', -1, false);
    if (parts.empty()) {
        throw LCompilersException("missing architecture in `--march`");
    }

#if LLVM_VERSION_MAJOR >= 18
    const llvm::AArch64::ArchInfo *arch
        = llvm::AArch64::parseArch(parts[0]);
    if (!arch) {
        throw LCompilersException("unsupported architecture '" + march
            + "' for target '" + triple.str()
            + "'; use `--mcpu` for processor names");
    }
    llvm::AArch64::ExtensionSet extensions;
    extensions.addArchDefaults(*arch);
    for (size_t i = 1; i < parts.size(); i++) {
        if (!extensions.parseModifier(parts[i])) {
            throw LCompilersException("unsupported architecture extension '"
                + parts[i].str() + "' in `--march=" + march + "`");
        }
    }
    return get_aarch64_extension_features(extensions);
#else
#if LLVM_VERSION_MAJOR >= 16
    const llvm::AArch64::ArchInfo &arch
        = llvm::AArch64::parseArch(parts[0]);
    if (arch.Name == "invalid") {
        throw LCompilersException("unsupported architecture '" + march
            + "' for target '" + triple.str()
            + "'; use `--mcpu` for processor names");
    }
    std::vector<llvm::StringRef> parsed_features = {
        arch.ArchFeature
    };
    llvm::AArch64::getExtensionFeatures(
        llvm::AArch64::getDefaultExtensions("", arch), parsed_features);
#else
    llvm::AArch64::ArchKind arch = llvm::AArch64::parseArch(parts[0]);
    if (arch == llvm::AArch64::ArchKind::INVALID) {
        throw LCompilersException("unsupported architecture '" + march
            + "' for target '" + triple.str()
            + "'; use `--mcpu` for processor names");
    }
    std::vector<llvm::StringRef> parsed_features;
    llvm::AArch64::getArchFeatures(arch, parsed_features);
    llvm::AArch64::getExtensionFeatures(
        llvm::AArch64::getDefaultExtensions("", arch), parsed_features);
#endif
    std::vector<std::string> features;
    features.reserve(parsed_features.size() + parts.size() - 1);
    for (llvm::StringRef feature: parsed_features) {
        features.push_back(feature.str());
    }
    for (size_t i = 1; i < parts.size(); i++) {
        llvm::StringRef extension = parts[i];
        bool disable = extension.consume_front("no");
        llvm::StringRef feature
            = llvm::AArch64::getArchExtFeature(extension);
        if (feature.empty()) {
            throw LCompilersException("unsupported architecture extension '"
                + parts[i].str() + "' in `--march=" + march + "`");
        }
        std::string feature_string = feature.str();
        if (disable) {
            if (!feature_string.empty() && feature_string[0] == '+') {
                feature_string[0] = '-';
            } else {
                feature_string = "-" + feature_string;
            }
        }
        features.push_back(feature_string);
    }
    return features;
#endif
#else
    (void)march;
    throw LCompilersException("`--march` is not supported for target '"
        + triple.str() + "' by this LFortran build");
#endif
}

std::vector<std::string> get_x86_arch_features(
    const std::string &march, const llvm::Triple &triple)
{
#ifdef HAVE_TARGET_X86
    bool only_64_bit = triple.getArch() == llvm::Triple::x86_64;
    if (llvm::X86::parseArchX86(march, only_64_bit)
            == llvm::X86::CK_None) {
        throw LCompilersException("unsupported architecture '" + march
            + "' for target '" + triple.str() + "'");
    }
    llvm::SmallVector<llvm::StringRef, 32> parsed_features;
#if LLVM_VERSION_MAJOR >= 22
    llvm::X86::getFeaturesForCPU(march, parsed_features, true);
#else
    llvm::X86::getFeaturesForCPU(march, parsed_features);
#endif
    std::vector<std::string> features;
    features.reserve(parsed_features.size());
    for (llvm::StringRef feature: parsed_features) {
#if LLVM_VERSION_MAJOR >= 22
        features.push_back(feature.str());
#else
        features.push_back("+" + feature.str());
#endif
    }
    return features;
#else
    (void)march;
    throw LCompilersException("`--march` is not supported for target '"
        + triple.str() + "' by this LFortran build");
#endif
}

std::vector<std::string> get_arch_features(
    const std::string &march, const llvm::Triple &triple,
    const LLVMTargetConfig &config)
{
    if (march == "native") {
        std::string host_cpu = get_host_cpu("--march", config);
        std::vector<std::string> features;
        if (triple.isAArch64()) {
            features = get_aarch64_cpu_features(host_cpu, triple);
        } else if (triple.getArch() == llvm::Triple::x86
                || triple.getArch() == llvm::Triple::x86_64) {
            features = get_x86_arch_features(host_cpu, triple);
        }
        std::vector<std::string> detected_features
            = get_detected_host_features();
        features.insert(features.end(), detected_features.begin(),
            detected_features.end());
        return features;
    }
    if (triple.isAArch64()) {
        return get_aarch64_arch_features(march, triple);
    }
    if (triple.getArch() == llvm::Triple::x86
            || triple.getArch() == llvm::Triple::x86_64) {
        return get_x86_arch_features(march, triple);
    }
    throw LCompilersException("`--march` is not supported for target '"
        + triple.str() + "'");
}

std::unique_ptr<llvm::TargetMachine> create_target_machine(
    const LLVMTargetConfig &config)
{
    const llvm::Target *target = get_llvm_target(config.triple);
    llvm::TargetOptions options;
#if LLVM_VERSION_MAJOR >= 8
    RM_OPTIONAL_TYPE<llvm::Reloc::Model> relocation_model
        = llvm::Reloc::Model::PIC_;
    RM_OPTIONAL_TYPE<llvm::CodeModel::Model> code_model;
    llvm::TargetMachine *machine = target->createTargetMachine(
#if LLVM_VERSION_MAJOR >= 21
        llvm::Triple(config.triple),
#else
        config.triple,
#endif
        config.cpu, config.features, options, relocation_model, code_model,
#if LLVM_VERSION_MAJOR >= 18
        config.fast ? llvm::CodeGenOptLevel::Aggressive
                    : llvm::CodeGenOptLevel::Default
#else
        config.fast ? llvm::CodeGenOpt::Aggressive
                    : llvm::CodeGenOpt::Default
#endif
    );
    if (!machine) {
        throw LCompilersException("could not create target machine for '"
            + config.triple + "'");
    }
    return std::unique_ptr<llvm::TargetMachine>(machine);
#else
    llvm::EngineBuilder builder;
    builder.setEngineKind(llvm::EngineKind::JIT);
    builder.setRelocationModel(llvm::Reloc::Model::PIC_);
    llvm::TargetMachine *machine = builder.selectTarget();
    if (!machine) {
        throw LCompilersException("could not create target machine for '"
            + config.triple + "'");
    }
    return std::unique_ptr<llvm::TargetMachine>(machine);
#endif
}

LLVMTargetConfig resolve_target_only(const std::string &target)
{
    CompilerOptions compiler_options;
    compiler_options.target = target;
    return resolve_llvm_target_config(compiler_options);
}

} // namespace

void LLVMTargetConfig::apply_target_attributes(llvm::Module &module) const
{
    for (llvm::Function &function: module) {
        if (function.isDeclaration()) {
            continue;
        }
        if (emit_cpu_attribute) {
            function.addFnAttr("target-cpu", cpu);
        }
        if (!features.empty()) {
            function.addFnAttr("target-features", features);
        }
        if (!tune_cpu.empty()) {
            function.addFnAttr("tune-cpu", tune_cpu);
        }
    }
}

LLVMTargetConfig resolve_llvm_target_config(
    const CompilerOptions &compiler_options)
{
    initialize_llvm_targets();

    LLVMTargetConfig config;
    config.fast = compiler_options.po.fast;
    config.triple = compiler_options.target.empty()
        ? llvm::sys::getDefaultTargetTriple()
        : llvm::Triple::normalize(compiler_options.target);
    llvm::Triple triple(config.triple);
    config.host_target = is_host_target(triple);
    const llvm::Target *target = get_llvm_target(config.triple);

    std::string march = compiler_options.march;
    std::string mcpu = compiler_options.mcpu;
    std::string mtune = compiler_options.mtune;
    if (config.fast && compiler_options.target.empty()
            && march.empty() && mcpu.empty() && mtune.empty()) {
        mcpu = "native";
    }

    std::vector<std::string> features;
    if (!march.empty()) {
        features = get_arch_features(march, triple, config);
    }

    std::string resolved_cpu;
    if (!mcpu.empty()) {
        resolved_cpu = mcpu == "native"
            ? get_host_cpu("--mcpu", config)
            : mcpu;
        validate_cpu(*target, triple, resolved_cpu, "--mcpu");
    }

    if (!march.empty()) {
        config.cpu = "generic";
        if (!resolved_cpu.empty()) {
            config.tune_cpu = resolved_cpu;
        }
    } else if (!resolved_cpu.empty()) {
        config.cpu = resolved_cpu;
        config.emit_cpu_attribute = true;
        if (mcpu == "native") {
            features = get_detected_host_features();
        }
    }

    if (!mtune.empty()) {
        config.tune_cpu = mtune == "native"
            ? get_host_cpu("--mtune", config)
            : mtune;
        validate_cpu(*target, triple, config.tune_cpu, "--mtune");
    }

    config.features = join_features(features);
    std::unique_ptr<llvm::TargetMachine> machine
        = create_target_machine(config);
    config.data_layout
        = machine->createDataLayout().getStringRepresentation();
    return config;
}

// Extracts the integer from APInt.
// APInt does not seem to have this functionality, so we implement it here.
uint64_t APInt_getint(const llvm::APInt &i) {
    // The APInt::isSingleWord() is private, but we can emulate it:
    bool isSingleWord = !i.needsCleanup();
    if (isSingleWord) {
        return *i.getRawData();
    } else {
        throw std::runtime_error("APInt too large to fit uint64_t");
    }
}

LLVMModule::LLVMModule(std::unique_ptr<llvm::Module> m)
{
    m_m = std::move(m);
}

LLVMModule::~LLVMModule() = default;

std::string LLVMModule::str()
{
    return LLVMEvaluator::module_to_string(*m_m);
}

llvm::Function *LLVMModule::get_function(const std::string &fn_name) {
    llvm::Module *m = m_m.get();
    return m->getFunction(fn_name);
}

llvm::GlobalVariable *LLVMModule::get_global(const std::string &global_name) {
    llvm::Module *m = m_m.get();
    return m->getNamedGlobal(global_name);
}

std::string LLVMModule::get_return_type(const std::string &fn_name)
{
    llvm::Module *m = m_m.get();
    llvm::Function *fn = m->getFunction(fn_name);
    if (!fn) {
        return "none";
    }
    llvm::Type *type = fn->getReturnType();
    if (type->isFloatTy()) {
        return "real4";
    } else if (type->isDoubleTy()) {
        return "real8";
    } else if (type->isIntegerTy(1)) {
        return "logical";
    } else if (type->isIntegerTy(32)) {
        return "integer4";
    } else if (type->isIntegerTy(64)) {
        return "integer8";
    } else if (type->isStructTy()) {
        llvm::StructType *st = llvm::cast<llvm::StructType>(type);
        if (st->hasName()) {
            if (startswith(std::string(st->getName()), "complex_4")) {
                return "complex4";
            } else if (startswith(std::string(st->getName()), "complex_8")) {
                return "complex8";
            } else {
                throw LCompilersException("LLVMModule::get_return_type(): StructType return type `" + std::string(st->getName()) + "` not supported");
            }
        } else {
            throw LCompilersException("LLVMModule::get_return_type(): Noname struct return type not supported");
        }
    } else if (type->isVectorTy()) {
        // Used for passing complex_4 on some platforms
        return "complex4";
    } else if (type->isVoidTy()) {
        return "void";
    } else {
        throw LCompilersException("LLVMModule::get_return_type(): Return type not supported");
    }
}

#ifdef HAVE_LFORTRAN_MLIR
MLIRModule::MLIRModule(std::unique_ptr<mlir::ModuleOp> m,
        std::unique_ptr<mlir::MLIRContext> ctx) {
    mlir_m = std::move(m);
    mlir_ctx = std::move(ctx);
    llvm_ctx = std::make_unique<llvm::LLVMContext>();
}

MLIRModule::~MLIRModule() {
    llvm_m.reset();
    llvm_ctx.reset();
};

std::string MLIRModule::mlir_str() {
    std::string mlir_str;
    llvm::raw_string_ostream raw_os(mlir_str);
    mlir_m->print(raw_os);
    return mlir_str;
}

std::string MLIRModule::llvm_str() {
    std::string mlir_str;
    llvm::raw_string_ostream raw_os(mlir_str);
    llvm_m->print(raw_os, nullptr);
    return mlir_str;
}

void MLIRModule::mlir_to_llvm(llvm::LLVMContext &ctx) {
    std::unique_ptr<llvm::Module> llvmModule = mlir::translateModuleToLLVMIR(
        *mlir_m, ctx);
    if (llvmModule) {
        llvm_m = std::move(llvmModule);
    } else {
        throw LCompilersException("Failed to generate LLVM IR");
    }
}
#endif

extern "C" {

float _lfortran_stan(float x);

}

LLVMEvaluator::LLVMEvaluator(const std::string &target)
    : LLVMEvaluator(resolve_target_only(target))
{
}

LLVMEvaluator::LLVMEvaluator(const CompilerOptions &compiler_options)
    : LLVMEvaluator(resolve_llvm_target_config(compiler_options))
{
}

LLVMEvaluator::LLVMEvaluator(LLVMTargetConfig target_config_)
    : target_config(std::move(target_config_))
{
    context = std::make_unique<llvm::LLVMContext>();
    TM = create_target_machine(target_config);

    // For some reason the JIT requires a different TargetMachine
    jit = cantFail(llvm::orc::KaleidoscopeJIT::Create());

    _lfortran_stan(0.5);
}

LLVMEvaluator::~LLVMEvaluator()
{
    jit.reset();
    context.reset();
}

void LLVMEvaluator::configure_module(llvm::Module &module) const
{
#if LLVM_VERSION_MAJOR >= 21
    module.setTargetTriple(llvm::Triple(target_config.triple));
#else
    module.setTargetTriple(target_config.triple);
#endif
    module.setDataLayout(target_config.data_layout);
    target_config.apply_target_attributes(module);
}

std::unique_ptr<llvm::Module> LLVMEvaluator::parse_module(const std::string &source, const std::string &filename="")
{
    llvm::SMDiagnostic err;
    std::unique_ptr<llvm::Module> module;
    if (!filename.empty()) {
        module = llvm::parseAssemblyFile(filename, err, *context);
    } else {
        module = llvm::parseAssemblyString(source, err, *context);
    }
    if (!module) {
        err.print("", llvm::errs());
        throw LCompilersException("parse_module(): Invalid LLVM IR");
    }
    bool v = llvm::verifyModule(*module);
    if (v) {
        throw LCompilersException("parse_module(): module failed verification.");
    };
    configure_module(*module);
    return module;
}

std::unique_ptr<LLVMModule> LLVMEvaluator::parse_module2(const std::string &source, const std::string &filename="") {
    return std::make_unique<LLVMModule>(parse_module(source, filename));
}

void LLVMEvaluator::add_module(const std::string &source) {
    std::unique_ptr<llvm::Module> module = parse_module(source);
    // TODO: apply LLVM optimizations here
    // Uncomment the below code to print the module to stdout:
    /*
    std::cout << "---------------------------------------------" << std::endl;
    std::cout << "LLVM Module IR:" << std::endl;
    std::cout << module_to_string(*module);
    std::cout << "---------------------------------------------" << std::endl;
    */
    add_module(std::move(module));
}

void LLVMEvaluator::add_module(std::unique_ptr<llvm::Module> mod) {
    // These are already set in parse_module(), but we set it here again for
    // cases when the Module was constructed directly, not via parse_module().
    configure_module(*mod);
    mod->setDataLayout(jit->getDataLayout());
    llvm::Error err = jit->addModule(std::move(mod), context);
    if (err) {
        llvm::SmallVector<char, 128> buf;
        llvm::raw_svector_ostream dest(buf);
        llvm::logAllUnhandledErrors(std::move(err), dest, "");
        std::string msg = std::string(dest.str().data(), dest.str().size());
        if (msg[msg.size()-1] == '\n') msg = msg.substr(0, msg.size()-1);
        throw LCompilersException("addModule() returned an error: " + msg);
    }

}

void LLVMEvaluator::add_module(std::unique_ptr<LLVMModule> m) {
    add_module(std::move(m->m_m));
}

intptr_t LLVMEvaluator::get_symbol_address(const std::string &name) {
#if LLVM_VERSION_MAJOR < 8
    // LLVM 7: Use findSymbol which returns JITSymbol
    llvm::JITSymbol s = jit->findSymbol(name);
    if (!s) {
        throw LCompilersException("findSymbol() failed to find the symbol '" + name + "'");
    }
    auto addr = s.getAddress();
    if (!addr) {
        llvm::Error e = addr.takeError();
        llvm::SmallVector<char, 128> buf;
        llvm::raw_svector_ostream dest(buf);
        llvm::logAllUnhandledErrors(std::move(e), dest, "");
        std::string msg = std::string(dest.str().data(), dest.str().size());
        if (msg[msg.size()-1] == '\n') msg = msg.substr(0, msg.size()-1);
        throw LCompilersException("getAddress() failed for symbol '"
            + name + "', error: " + msg);
    }
    return (intptr_t)addr.get();
#else
#if LLVM_VERSION_MAJOR < 17
    llvm::Expected<llvm::JITEvaluatedSymbol>
#else
    llvm::Expected<llvm::orc::ExecutorSymbolDef>
#endif
        s = jit->lookup(name);
    if (!s) {
        llvm::Error e = s.takeError();
        llvm::SmallVector<char, 128> buf;
        llvm::raw_svector_ostream dest(buf);
        llvm::logAllUnhandledErrors(std::move(e), dest, "");
        std::string msg = std::string(dest.str().data(), dest.str().size());
        if (msg[msg.size()-1] == '\n') msg = msg.substr(0, msg.size()-1);
        throw LCompilersException("lookup() failed to find the symbol '"
            + name + "', error: " + msg);
    }
#if LLVM_VERSION_MAJOR < 17
    llvm::Expected<uint64_t> addr0 = s->getAddress();
#else
    llvm::Expected<uint64_t> addr0 = s->getAddress().getValue();
#endif
    if (!addr0) {
        llvm::Error e = addr0.takeError();
        llvm::SmallVector<char, 128> buf;
        llvm::raw_svector_ostream dest(buf);
        llvm::logAllUnhandledErrors(std::move(e), dest, "");
        std::string msg = std::string(dest.str().data(), dest.str().size());
        if (msg[msg.size()-1] == '\n') msg = msg.substr(0, msg.size()-1);
        throw LCompilersException("JITSymbol::getAddress() returned an error: " + msg);
    }
    return (intptr_t)cantFail(std::move(addr0));
#endif
}

void write_file(const std::string &filename, const std::string &contents)
{
    std::ofstream out;
    out.open(filename);
    out << contents << std::endl;
}

std::string LLVMEvaluator::get_asm(llvm::Module &m)
{
    configure_module(m);
    llvm::legacy::PassManager pass;
#if LLVM_VERSION_MAJOR < 10
    llvm::LLVMTargetMachine::CodeGenFileType ft = llvm::LLVMTargetMachine::CGFT_AssemblyFile;
#elif LLVM_VERSION_MAJOR < 18
    llvm::CodeGenFileType ft = llvm::CGFT_AssemblyFile;
#else
    llvm::CodeGenFileType ft = llvm::CodeGenFileType::AssemblyFile;
#endif
    llvm::SmallVector<char, 128> buf;
    llvm::raw_svector_ostream dest(buf);
    if (TM->addPassesToEmitFile(pass, dest, nullptr, ft)) {
        throw std::runtime_error("TargetMachine can't emit a file of this type");
    }
    pass.run(m);
    return std::string(dest.str().data(), dest.str().size());
}

void LLVMEvaluator::save_asm_file(llvm::Module &m, const std::string &filename)
{
    write_file(filename, get_asm(m));
}

void LLVMEvaluator::save_object_file(llvm::Module &m, const std::string &filename) {
    configure_module(m);

    llvm::legacy::PassManager pass;
#if LLVM_VERSION_MAJOR < 10
    llvm::LLVMTargetMachine::CodeGenFileType ft = llvm::LLVMTargetMachine::CGFT_ObjectFile;
#elif LLVM_VERSION_MAJOR < 18
    llvm::CodeGenFileType ft = llvm::CGFT_ObjectFile;
#else
    llvm::CodeGenFileType ft = llvm::CodeGenFileType::ObjectFile;
#endif
    std::error_code EC;
    llvm::raw_fd_ostream dest(filename, EC, llvm::sys::fs::OF_None);
    if (EC) {
        throw std::runtime_error("raw_fd_ostream failed");
    }
    if (TM->addPassesToEmitFile(pass, dest, nullptr, ft)) {
        throw std::runtime_error("TargetMachine can't emit a file of this type");
    }
    pass.run(m);
    dest.flush();
}

void LLVMEvaluator::create_empty_object_file(const std::string &filename) {
    std::string source;
    std::unique_ptr<llvm::Module> module = parse_module(source);
    save_object_file(*module, filename);
}

void LLVMEvaluator::opt(llvm::Module &m) {
    configure_module(m);

#if LLVM_VERSION_MAJOR >= 17
    llvm::LoopAnalysisManager LAM;
    llvm::FunctionAnalysisManager FAM;
    llvm::CGSCCAnalysisManager CGAM;
    llvm::ModuleAnalysisManager MAM;
    llvm::PassBuilder PB = llvm::PassBuilder(TM.get());
    PB.registerModuleAnalyses(MAM);
    PB.registerCGSCCAnalyses(CGAM);
    PB.registerFunctionAnalyses(FAM);
    PB.registerLoopAnalyses(LAM);
    PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);
    llvm::ModulePassManager MPM = PB.buildPerModuleDefaultPipeline(llvm::OptimizationLevel::O3);
    MPM.run(m, MAM);

#else
    llvm::legacy::PassManager mpm;
    mpm.add(new llvm::TargetLibraryInfoWrapperPass(TM->getTargetTriple()));
    mpm.add(llvm::createTargetTransformInfoWrapperPass(TM->getTargetIRAnalysis()));
    llvm::legacy::FunctionPassManager fpm(&m);
    fpm.add(llvm::createTargetTransformInfoWrapperPass(TM->getTargetIRAnalysis()));
    int optLevel = 3;
    int sizeLevel = 0;
    llvm::PassManagerBuilder builder;
    builder.OptLevel = optLevel;
    builder.SizeLevel = sizeLevel;
    builder.Inliner = llvm::createFunctionInliningPass(optLevel, sizeLevel,
        false);
    builder.DisableUnrollLoops = false;
    builder.LoopVectorize = true;
    builder.SLPVectorize = true;
    builder.populateFunctionPassManager(fpm);
    builder.populateModulePassManager(mpm);
    fpm.doInitialization();
    for (llvm::Function &func : m) {
        fpm.run(func);
    }
    fpm.doFinalization();
    mpm.add(llvm::createVerifierPass());
    mpm.run(m);
#endif
}

std::string LLVMEvaluator::module_to_string(llvm::Module &m) {
    std::string buf;
    llvm::raw_string_ostream os(buf);
    m.print(os, nullptr);
    os.flush();
    return buf;
}

void LLVMEvaluator::print_version_message()
{
    llvm::cl::PrintVersionMessage();
}

std::string LLVMEvaluator::llvm_version()
{
    return LLVM_VERSION_STRING;
}

llvm::LLVMContext &LLVMEvaluator::get_context()
{
    return *context;
}

const llvm::DataLayout &LLVMEvaluator::get_jit_data_layout() {
    return jit->getDataLayout();
}

void LLVMEvaluator::print_targets()
{
    llvm::InitializeNativeTarget();
#ifdef HAVE_TARGET_AARCH64
    LLVMInitializeAArch64TargetInfo();
#endif
#ifdef HAVE_TARGET_X86
    LLVMInitializeX86TargetInfo();
#endif
#ifdef HAVE_TARGET_WASM
    LLVMInitializeWebAssemblyTargetInfo();
#endif
    llvm::raw_ostream &os = llvm::outs();
    llvm::TargetRegistry::printRegisteredTargetsForVersion(os);
}

std::string LLVMEvaluator::get_default_target_triple()
{
    return LLVMGetDefaultTargetTriple();
}

#ifdef __EMSCRIPTEN__

WasmLFortranExecutor::~WasmLFortranExecutor() = default;

WasmLFortranExecutor::WasmLFortranExecutor()
{
    LLVMInitializeWebAssemblyTarget();
    LLVMInitializeWebAssemblyTargetInfo();
    LLVMInitializeWebAssemblyTargetMC();
    LLVMInitializeWebAssemblyAsmPrinter();
    LLVMInitializeWebAssemblyAsmParser();

    context = std::make_unique<llvm::LLVMContext>();

    llvm::SmallString<256> tmp;
    if (llvm::sys::fs::createUniqueDirectory("xlfortran-wasm-exec-", tmp))
        throw LCompilersException("WasmLFortranExecutor: failed to create temp dir");
    TempDir = tmp.str().str();

    // Claim a unique instance ID so that __lfortran_evaluate_N function names
    // are globally unique across all executor instances in the same process.
    static std::atomic<int> s_next_id{0};
    m_id = s_next_id.fetch_add(1);
}

llvm::LLVMContext &WasmLFortranExecutor::get_context()
{
    return *context;
}

std::unique_ptr<LLVMModule> WasmLFortranExecutor::parse_module2(
    const std::string &source, const std::string &/*filename*/)
{
    llvm::SMDiagnostic err;
    auto mod = llvm::parseAssemblyString(source, err, *context);
    if (!mod)
        throw LCompilersException("WasmLFortranExecutor::parse_module2: Invalid LLVM IR: "
                                  + err.getMessage().str());
    return std::make_unique<LLVMModule>(std::move(mod));
}

void WasmLFortranExecutor::add_module(std::unique_ptr<LLVMModule> lm, int eval_count)
{
    std::unique_ptr<llvm::Module> mod = std::move(lm->m_m);

    // Rename __lfortran_evaluate_<eval_count> → __lfortran_evaluate_<m_id>_<eval_count>
    // so that multiple WasmLFortranExecutor instances in the same process (e.g.
    // test suite) each own uniquely-named symbols and dlsym(RTLD_DEFAULT) finds
    // the right one without needing per-handle tracking.
    const std::string logical_stem = "__lfortran_evaluate_" + std::to_string(eval_count);
    const std::string unique_stem  = "__lfortran_evaluate_" + std::to_string(m_id)
                                     + "_" + std::to_string(eval_count);
    if (llvm::Function *fn = mod->getFunction(logical_stem))
        fn->setName(unique_stem);

    const std::string triple = "wasm32-unknown-emscripten";
    std::string err;
#if LLVM_VERSION_MAJOR >= 21
    const llvm::Target *Target = llvm::TargetRegistry::lookupTarget(llvm::Triple(triple), err);
#else
    const llvm::Target *Target = llvm::TargetRegistry::lookupTarget(triple, err);
#endif
    if (!Target)
        throw LCompilersException("WasmLFortranExecutor: failed to find wasm32 target: " + err);

    llvm::TargetOptions TO;
    std::unique_ptr<llvm::TargetMachine> TM(
#if LLVM_VERSION_MAJOR >= 21
        Target->createTargetMachine(llvm::Triple(triple), "", "", TO, llvm::Reloc::Model::PIC_)
#else
        Target->createTargetMachine(triple, "", "", TO, llvm::Reloc::Model::PIC_)
#endif
    );
    mod->setTargetTriple(llvm::Triple(triple));
    mod->setDataLayout(TM->createDataLayout());

    llvm::SmallString<256> objFile, wasmFile;
    objFile = TempDir;
    llvm::sys::path::append(objFile, unique_stem + ".o");
    wasmFile = TempDir;
    llvm::sys::path::append(wasmFile, unique_stem + ".wasm");

    std::error_code EC;
    llvm::raw_fd_ostream ObjOut(objFile, EC);
    if (EC)
        throw LCompilersException("WasmLFortranExecutor: cannot open obj file: " + EC.message());

    llvm::legacy::PassManager PM;
    if (TM->addPassesToEmitFile(PM, ObjOut, nullptr, llvm::CodeGenFileType::ObjectFile))
        throw LCompilersException("WasmLFortranExecutor: wasm32 backend cannot emit object file");
    PM.run(*mod);
    ObjOut.close();

    std::vector<const char *> LinkerArgs = {
        "wasm-ld", "-shared",
        "--import-memory", "--experimental-pic", "--stack-first", "--allow-undefined",
        objFile.c_str(), "-o", wasmFile.c_str()
    };
    std::string lld_errs;
    llvm::raw_string_ostream lld_errs_stream(lld_errs);
    const ::lld::DriverDef WasmDriver = {::lld::Flavor::Wasm, &::lld::wasm::link};
    ::lld::Result Result = ::lld::lldMain(LinkerArgs, llvm::outs(), lld_errs_stream, {WasmDriver});
    if (Result.retCode)
        throw LCompilersException("WasmLFortranExecutor: wasm-ld failed for "
                                  + unique_stem + ": " + lld_errs_stream.str());

    void *handle = dlopen(wasmFile.c_str(), RTLD_NOW | RTLD_GLOBAL);
    if (!handle)
        throw LCompilersException(std::string("WasmLFortranExecutor: dlopen failed: ") + dlerror());
    (void)handle; // side module stays live for the process lifetime via RTLD_GLOBAL
}

intptr_t WasmLFortranExecutor::get_symbol_address(const std::string &name)
{
    // Translate the logical name to the instance-unique name, then look it up
    // in the global RTLD symbol table.  Within one session there is only ever
    // one executor, so RTLD_DEFAULT finds the right symbol.  In the test suite
    // the per-instance prefix prevents collisions across test cases.
    const std::string prefix = "__lfortran_evaluate_";
    std::string actual = name;
    if (name.substr(0, prefix.size()) == prefix)
        actual = prefix + std::to_string(m_id) + "_" + name.substr(prefix.size());

    void *sym = dlsym(RTLD_DEFAULT, actual.c_str());
    if (!sym)
        throw LCompilersException("WasmLFortranExecutor: symbol not found: " + name);
    return reinterpret_cast<intptr_t>(sym);
}

#endif // __EMSCRIPTEN__

} // namespace LCompilers
