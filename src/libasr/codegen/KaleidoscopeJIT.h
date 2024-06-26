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
#include "llvm/ExecutionEngine/Orc/JITTargetMachineBuilder.h"
#include "llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/LLVMContext.h"
#include <memory>

#if LLVM_VERSION_MAJOR < 12
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <libasr/exception.h>
#endif

#if LLVM_VERSION_MAJOR >= 13
#include "llvm/ExecutionEngine/Orc/ExecutorProcessControl.h"
#endif

#if LLVM_VERSION_MAJOR >= 16
#    define RM_OPTIONAL_TYPE std::optional
#else
#    define RM_OPTIONAL_TYPE llvm::Optional
#endif

namespace llvm {
namespace orc {

class KaleidoscopeJIT {

#if LLVM_VERSION_MAJOR < 12
  using ObjLayerT = LegacyRTDyldObjectLinkingLayer;
  using CompileLayerT = LegacyIRCompileLayer<ObjLayerT, SimpleCompiler>;
#endif

private:
  std::unique_ptr<ExecutionSession> ES;
#if LLVM_VERSION_MAJOR >= 12
  RTDyldObjectLinkingLayer ObjectLayer;
  IRCompileLayer CompileLayer;
  DataLayout DL;
  MangleAndInterner Mangle;
  JITDylib &JITDL;
#else
  std::shared_ptr<SymbolResolver> Resolver;
  std::unique_ptr<TargetMachine> TM;
  ObjLayerT ObjectLayer;
  CompileLayerT CompileLayer;
  std::vector<VModuleKey> ModuleKeys;
#endif
  DataLayout DL;

public:
#if LLVM_VERSION_MAJOR >= 12
  KaleidoscopeJIT(std::unique_ptr<ExecutionSession> ES, JITTargetMachineBuilder JTMB, DataLayout DL)
      :
        ES(std::move(ES)),
#else
  KaleidoscopeJIT()
      :
        ES(std::make_unique<ExecutionSession>()),
#endif
#if LLVM_VERSION_MAJOR >= 12
        ObjectLayer(*this->ES,
                    []() { return std::make_unique<SectionMemoryManager>(); }),
        CompileLayer(*this->ES, ObjectLayer, std::make_unique<ConcurrentIRCompiler>(std::move(JTMB))),
        DL(std::move(DL)), Mangle(*this->ES, this->DL),
        JITDL(cantFail(this->ES->createJITDylib("Main")))
#else
        Resolver(createLegacyLookupResolver(
            *ES,
            [this](StringRef Name) {
              return lookup(std::string(Name));
            },
            [](Error Err) { cantFail(std::move(Err), "lookupFlags failed"); })),
        TM(EngineBuilder().selectTarget()),
        ObjectLayer(AcknowledgeORCv1Deprecation, *ES,
                    [this](VModuleKey) {
                      return ObjLayerT::Resources{
                          std::make_shared<SectionMemoryManager>(), Resolver};
                    }),
        CompileLayer(AcknowledgeORCv1Deprecation, ObjectLayer,
                     SimpleCompiler(*TM)),
        DL(TM->createDataLayout())
#endif
         {
#if LLVM_VERSION_MAJOR >= 12
    JITDL.addGenerator(
        cantFail(DynamicLibrarySearchGenerator::GetForCurrentProcess(
            DL.getGlobalPrefix())));
    if (JTMB.getTargetTriple().isOSBinFormatCOFF()) {
      ObjectLayer.setOverrideObjectFlagsWithResponsibilityFlags(true);
      ObjectLayer.setAutoClaimResponsibilityForObjectSymbols(true);
    }
#else
    llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);
#endif
  }

#if LLVM_VERSION_MAJOR >= 12
  static Expected<std::unique_ptr<KaleidoscopeJIT>> Create() {
#if LLVM_VERSION_MAJOR >= 13
    auto EPC = SelfExecutorProcessControl::Create();
    if (!EPC)
      return EPC.takeError();

    auto ES = std::make_unique<ExecutionSession>(std::move(*EPC));

    JITTargetMachineBuilder JTMB(
        ES->getExecutorProcessControl().getTargetTriple());
#else
    auto ES = std::make_unique<ExecutionSession>();

    auto JTMB_P = JITTargetMachineBuilder::detectHost();
    if (!JTMB_P)
      return JTMB_P.takeError();

    auto JTMB = *JTMB_P;
#endif

    auto DL = JTMB.getDefaultDataLayoutForTarget();
    if (!DL)
      return DL.takeError();

    return std::make_unique<KaleidoscopeJIT>(std::move(ES), std::move(JTMB),
                                             std::move(*DL));
  }
#endif

  const DataLayout &getDataLayout() const { return DL; }

  // JITDylib &getMainJITDylib() { return JITDL; }

#if LLVM_VERSION_MAJOR >= 12
  Error addModule(std::unique_ptr<Module> M, std::unique_ptr<LLVMContext> Ctx, ResourceTrackerSP RT) {
    auto res =  CompileLayer.add(RT,
                            ThreadSafeModule(std::move(M), std::move(Ctx)));
    return res;
  }
#else
  VModuleKey addModule(std::unique_ptr<Module> M) {
    auto K = ES->allocateVModule();
    cantFail(CompileLayer.addModule(K, std::move(M)));
    ModuleKeys.push_back(K);
    return K;
  }

  void removeModule(VModuleKey K) {
    ModuleKeys.erase(find(ModuleKeys, K));
    cantFail(CompileLayer.removeModule(K));
  }
#endif

#if LLVM_VERSION_MAJOR >= 12
  Expected<JITEvaluatedSymbol> lookup(StringRef Name) {
#if LLVM_VERSION_MAJOR >= 17
    // TODO
#else
    return ES->lookup({&JITDL}, Mangle(Name.str()));
#endif
  }
#else
  JITSymbol lookup(const std::string &Name) {
#ifdef _WIN32
    // The symbol lookup of ObjectLinkingLayer uses the SymbolRef::SF_Exported
    // flag to decide whether a symbol will be visible or not, when we call
    // IRCompileLayer::findSymbolIn with ExportedSymbolsOnly set to true.
    //
    // But for Windows COFF objects, this flag is currently never set.
    // For a potential solution see: https://reviews.llvm.org/rL258665
    // For now, we allow non-exported symbols on Windows as a workaround.
    const bool ExportedSymbolsOnly = false;
#else
    const bool ExportedSymbolsOnly = true;
#endif

    // Search modules in reverse order: from last added to first added.
    // This is the opposite of the usual search order for dlsym, but makes more
    // sense in a REPL where we want to bind to the newest available definition.
    for (auto H : make_range(ModuleKeys.rbegin(), ModuleKeys.rend()))
      if (auto Sym = CompileLayer.findSymbolIn(H, Name, ExportedSymbolsOnly))
        return Sym;

    // If we can't find the symbol in the JIT, try looking in the host process.
    if (auto SymAddr = RTDyldMemoryManager::getSymbolAddressInProcess(Name))
      return JITSymbol(SymAddr, JITSymbolFlags::Exported);

#ifdef _WIN32
    // For Windows retry without "_" at beginning, as RTDyldMemoryManager uses
    // GetProcAddress and standard libraries like msvcrt.dll use names
    // with and without "_" (for example "_itoa" but "sin").
    if (Name.length() > 2 && Name[0] == '_')
      if (auto SymAddr =
              RTDyldMemoryManager::getSymbolAddressInProcess(Name.substr(1)))
        return JITSymbol(SymAddr, JITSymbolFlags::Exported);
#endif

    return nullptr;
  }
#endif
};

} // end namespace orc
} // end namespace llvm

#endif // LLVM_EXECUTIONENGINE_ORC_KALEIDOSCOPEJIT_H
