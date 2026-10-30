//===- KaleidoscopeJIT.h - A simple JIT for Kaleidoscope --------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Contains a simple JIT definition for use in the kaleidoscope tutorials.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_EXECUTIONENGINE_ORC_KALEIDOSCOPEJIT_H
#define LLVM_EXECUTIONENGINE_ORC_KALEIDOSCOPEJIT_H

#include "llvm/ADT/StringRef.h"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/CompileUtils.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/IRCompileLayer.h"
#if LLVM_VERSION_MAJOR >= 8
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#endif
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Support/MathExtras.h"
#if LLVM_VERSION_MAJOR < 8
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/Orc/Legacy.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/DynamicLibrary.h"
#include "llvm/IR/Mangler.h"
#endif
#include <cassert>
#include <cstdint>
#include <cstdlib>
#include <memory>

#if LLVM_VERSION_MAJOR >= 13
#include "llvm/ExecutionEngine/Orc/ExecutorProcessControl.h"
#endif

#if LLVM_VERSION_MAJOR >= 21
#include "llvm/ExecutionEngine/Orc/SelfExecutorProcessControl.h"
#endif

// absoluteSymbols() moved out of Core.h in LLVM 20.
#if LLVM_VERSION_MAJOR >= 20
#include "llvm/ExecutionEngine/Orc/AbsoluteSymbols.h"
#endif

#if LLVM_VERSION_MAJOR >= 16
#    define RM_OPTIONAL_TYPE std::optional
#else
#    define RM_OPTIONAL_TYPE llvm::Optional
#endif

namespace llvm {
namespace orc {

// The GNU unwinder indexes an object's entire min/max PC range. Keep its code
// sections together so another object's code cannot fall inside that range.
class ContiguousSectionMemoryManager final : public SectionMemoryManager {
  uint8_t *CodeBase = nullptr;
  uintptr_t CodeSize = 0;
  uintptr_t CodeOffset = 0;
#if LLVM_VERSION_MAJOR >= 16
  using SectionAlignment = Align;
#else
  using SectionAlignment = uint32_t;
#endif

public:
  bool needsToReserveAllocationSpace() override { return true; }

  void reserveAllocationSpace(uintptr_t Size, SectionAlignment CodeAlign,
                              uintptr_t, SectionAlignment,
                              uintptr_t, SectionAlignment) override {
    assert(!CodeBase && "one code reservation per object");
    CodeSize = Size;
    if (Size == 0)
      return;
#if LLVM_VERSION_MAJOR >= 16
    unsigned Alignment = CodeAlign.value();
#else
    unsigned Alignment = CodeAlign;
#endif
    CodeBase = SectionMemoryManager::allocateCodeSection(Size, Alignment, 0, "");
  }

  uint8_t *allocateCodeSection(uintptr_t Size, unsigned Alignment,
                               unsigned, StringRef) override {
    if (!CodeBase)
      return nullptr;
    uintptr_t Offset = alignTo(CodeOffset, Alignment ? Alignment : 16);
    assert(Offset <= CodeSize && Size <= CodeSize - Offset);
    CodeOffset = Offset + Size;
    return CodeBase + Offset;
  }
};

class KaleidoscopeJIT {
private:
#if LLVM_VERSION_MAJOR >= 8
  std::unique_ptr<ExecutionSession> ES;
  RTDyldObjectLinkingLayer ObjectLayer;
#if LLVM_VERSION_MAJOR < 10
  // SimpleCompiler only keeps a reference to the TargetMachine, so it must
  // outlive CompileLayer.
  std::unique_ptr<TargetMachine> TM;
#endif
  IRCompileLayer CompileLayer;

  DataLayout DL;
  MangleAndInterner Mangle;
  JITDylib &JITDL;
#else
  // LLVM 7: Different JIT infrastructure
  ExecutionSession ES;
  std::shared_ptr<SymbolResolver> Resolver;
  std::unique_ptr<TargetMachine> TM;
  const DataLayout DL;
  RTDyldObjectLinkingLayer ObjectLayer;
  IRCompileLayer<decltype(ObjectLayer), SimpleCompiler> CompileLayer;
#endif

public:
#if LLVM_VERSION_MAJOR >= 8
  KaleidoscopeJIT(std::unique_ptr<ExecutionSession> ES, JITTargetMachineBuilder JTMB, DataLayout DL)
      :
        ES(std::move(ES)),
#if LLVM_VERSION_MAJOR >= 21
        ObjectLayer(*this->ES,
                    [](const llvm::MemoryBuffer &) { return std::make_unique<ContiguousSectionMemoryManager>(); }),
#else
        ObjectLayer(*this->ES,
                    []() { return std::make_unique<ContiguousSectionMemoryManager>(); }),
#endif
#if LLVM_VERSION_MAJOR >= 10
        CompileLayer(*this->ES, ObjectLayer, std::make_unique<ConcurrentIRCompiler>(std::move(JTMB))),
#else
        TM(cantFail(JTMB.createTargetMachine())),
        CompileLayer(*this->ES, ObjectLayer, SimpleCompiler(*TM)),
#endif
        DL(std::move(DL)), Mangle(*this->ES, this->DL),
        JITDL(
#if LLVM_VERSION_MAJOR >= 11
            cantFail
#endif
          (this->ES->createJITDylib("Main"))) {
#if LLVM_VERSION_MAJOR >= 10
    JITDL.addGenerator(
        cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(
            DL.getGlobalPrefix())));
#elif LLVM_VERSION_MAJOR >= 9
    JITDL.setGenerator(
        cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(
            DL.getGlobalPrefix())));
#else
    // LLVM 8: GetForCurrentProcess takes DataLayout reference, not global prefix
    JITDL.setGenerator(
        cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(
            this->DL)));
#endif
    if (JTMB.getTargetTriple().isOSBinFormatCOFF()) {
      ObjectLayer.setOverrideObjectFlagsWithResponsibilityFlags(true);
      ObjectLayer.setAutoClaimResponsibilityForObjectSymbols(true);
    }
#ifdef _WIN32
    // STOP calls exit(), which must be the host's, whose atexit handlers run:
    // the search of the process can find the exit() of another C runtime,
    // such as msvcrt.dll's, which does not run them.
    defineHostSymbol("exit", reinterpret_cast<void *>(&::exit));
#endif
  }

  // Resolves `name` in the code it compiles to `address` in the host.
  void defineHostSymbol(StringRef name, void *address) {
    SymbolMap symbols;
#if LLVM_VERSION_MAJOR >= 17
    symbols[Mangle(name)] = {ExecutorAddr::fromPtr(address), JITSymbolFlags::Exported};
#else
    symbols[Mangle(name)] = JITEvaluatedSymbol(
        static_cast<JITTargetAddress>(reinterpret_cast<uintptr_t>(address)),
        JITSymbolFlags::Exported);
#endif
    cantFail(JITDL.define(absoluteSymbols(std::move(symbols))));
  }
#else
  // LLVM 7: Constructor takes TargetMachine directly, different API structure
  KaleidoscopeJIT(std::unique_ptr<TargetMachine> TM_)
      : Resolver(createLegacyLookupResolver(
            ES,
            [this](const std::string &Name) {
              return findMangledSymbol(Name);
            },
            [](Error Err) { cantFail(std::move(Err), "lookupFlags failed"); })),
        TM(std::move(TM_)), DL(TM->createDataLayout()),
        ObjectLayer(ES, [this](VModuleKey) {
          return RTDyldObjectLinkingLayer::Resources{
              std::make_shared<ContiguousSectionMemoryManager>(), Resolver};
        }),
        CompileLayer(ObjectLayer, SimpleCompiler(*TM)) {
    llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);
  }
#endif

#if LLVM_VERSION_MAJOR >= 18
  using OptLevel = CodeGenOptLevel;
#else
  using OptLevel = CodeGenOpt::Level;
#endif

  static Expected<std::unique_ptr<KaleidoscopeJIT>> Create(OptLevel opt_level) {
#if LLVM_VERSION_MAJOR >= 13
    auto EPC = SelfExecutorProcessControl::Create();
    if (!EPC)
      return EPC.takeError();

    auto ES = std::make_unique<ExecutionSession>(std::move(*EPC));

    JITTargetMachineBuilder JTMB(
        ES->getExecutorProcessControl().getTargetTriple());
#elif LLVM_VERSION_MAJOR >= 8
    auto ES = std::make_unique<ExecutionSession>();

    auto JTMB_P = JITTargetMachineBuilder::detectHost();
    if (!JTMB_P)
      return JTMB_P.takeError();

    auto JTMB = *JTMB_P;
#else
    // LLVM 7: Create TargetMachine directly using EngineBuilder
    auto JTMB = EngineBuilder().setOptLevel(opt_level).selectTarget();
    if (!JTMB)
      return make_error<StringError>("Could not create target machine",
                                     inconvertibleErrorCode());
    
    return std::make_unique<KaleidoscopeJIT>(std::unique_ptr<TargetMachine>(JTMB));
#endif

#if LLVM_VERSION_MAJOR >= 8
    JTMB.setCodeGenOptLevel(opt_level);
    auto DL = JTMB.getDefaultDataLayoutForTarget();
    if (!DL)
      return DL.takeError();

    return std::make_unique<KaleidoscopeJIT>(std::move(ES), std::move(JTMB),
                                             std::move(*DL));
#endif
  }

  const DataLayout &getDataLayout() const { return DL; }

#if LLVM_VERSION_MAJOR >= 8
  Error addModule(std::unique_ptr<Module> M, std::unique_ptr<LLVMContext> &Ctx) {
    auto res =  CompileLayer.add(JITDL,
                            ThreadSafeModule(std::move(M), std::move(Ctx)));
    Ctx = std::make_unique<LLVMContext>();
    return res;
  }
#else
  // LLVM 7: Different addModule API
  Error addModule(std::unique_ptr<Module> M, std::unique_ptr<LLVMContext> & /* Ctx */) {
    auto K = ES.allocateVModule();
    return CompileLayer.addModule(K, std::move(M));
  }
#endif

#if LLVM_VERSION_MAJOR >= 8
#if LLVM_VERSION_MAJOR < 17
  Expected<JITEvaluatedSymbol> lookup(StringRef Name) {
#else
  Expected<ExecutorSymbolDef> lookup(StringRef Name) {
#endif
    return ES->lookup({&JITDL}, Mangle(Name.str()));
  }
#else
  // LLVM 7: Different lookup API
  JITSymbol findSymbol(const std::string Name) {
    std::string MangledName;
    raw_string_ostream MangledNameStream(MangledName);
    Mangler::getNameWithPrefix(MangledNameStream, Name, DL);
    return findMangledSymbol(MangledNameStream.str());
  }

private:
  // Search the JIT'd modules first, then the symbols of the host process
  // (e.g. the LFortran runtime library linked into the compiler).
  JITSymbol findMangledSymbol(const std::string &Name) {
    if (auto Sym = CompileLayer.findSymbol(Name, false))
      return Sym;
    else if (auto Err = Sym.takeError())
      return std::move(Err);
    if (auto SymAddr = RTDyldMemoryManager::getSymbolAddressInProcess(Name))
      return JITSymbol(SymAddr, JITSymbolFlags::Exported);
    return nullptr;
  }
#endif

};

} // end namespace orc
} // end namespace llvm

#endif // LLVM_EXECUTIONENGINE_ORC_KALEIDOSCOPEJIT_H
