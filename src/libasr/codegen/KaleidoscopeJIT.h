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
#if LLVM_VERSION_MAJOR < 8
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/DynamicLibrary.h"
#include "llvm/IR/Mangler.h"
#endif
#include <memory>

#if LLVM_VERSION_MAJOR >= 13
#include "llvm/ExecutionEngine/Orc/ExecutorProcessControl.h"
#endif

#if LLVM_VERSION_MAJOR >= 21
#include "llvm/ExecutionEngine/Orc/SelfExecutorProcessControl.h"
#endif

#if LLVM_VERSION_MAJOR >= 16
#    define RM_OPTIONAL_TYPE std::optional
#else
#    define RM_OPTIONAL_TYPE llvm::Optional
#endif

namespace llvm {
namespace orc {

class KaleidoscopeJIT {
private:
#if LLVM_VERSION_MAJOR >= 8
  std::unique_ptr<ExecutionSession> ES;
  RTDyldObjectLinkingLayer ObjectLayer;
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
                    [](const llvm::MemoryBuffer &) { return std::make_unique<SectionMemoryManager>(); }),
#else
        ObjectLayer(*this->ES,
                    []() { return std::make_unique<SectionMemoryManager>(); }),
#endif
#if LLVM_VERSION_MAJOR >= 10
        CompileLayer(*this->ES, ObjectLayer, std::make_unique<ConcurrentIRCompiler>(std::move(JTMB))),
#else
        CompileLayer(*this->ES, ObjectLayer, SimpleCompiler(**JTMB.createTargetMachine())),
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
  }
#else
  // LLVM 7: Constructor takes TargetMachine directly, different API structure
  KaleidoscopeJIT(std::unique_ptr<TargetMachine> TM_)
      : TM(std::move(TM_)), DL(TM->createDataLayout()),
        ObjectLayer(ES, [](VModuleKey) {
          return RTDyldObjectLinkingLayer::Resources{
              std::make_shared<SectionMemoryManager>(), nullptr};
        }),
        CompileLayer(ObjectLayer, SimpleCompiler(*TM)) {
    llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);
  }
#endif

  static Expected<std::unique_ptr<KaleidoscopeJIT>> Create() {
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
    auto JTMB = EngineBuilder().selectTarget();
    if (!JTMB)
      return make_error<StringError>("Could not create target machine",
                                     inconvertibleErrorCode());
    
    return std::make_unique<KaleidoscopeJIT>(std::unique_ptr<TargetMachine>(JTMB));
#endif

#if LLVM_VERSION_MAJOR >= 8
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
    return CompileLayer.findSymbol(MangledNameStream.str(), false);
  }
#endif

};

} // end namespace orc
} // end namespace llvm

#endif // LLVM_EXECUTIONENGINE_ORC_KALEIDOSCOPEJIT_H
