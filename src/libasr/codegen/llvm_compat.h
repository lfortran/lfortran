#ifndef LIBASR_CODEGEN_LLVM_COMPAT_H
#define LIBASR_CODEGEN_LLVM_COMPAT_H

// LLVM compatibility layer for supporting multiple LLVM versions

// LLVM 9 compatibility: MaybeAlign and Align were added in LLVM 10
#if LLVM_VERSION_MAJOR < 10
namespace llvm {
    struct MaybeAlign {
        MaybeAlign() : value(0) {}
        explicit MaybeAlign(unsigned v) : value(v) {}
        operator unsigned() const { return value; }
        unsigned value;
    };
    struct Align {
        explicit Align(unsigned v) : value(v) {}
        operator unsigned() const { return value; }
        unsigned value;
    };
}
#endif

#endif // LIBASR_CODEGEN_LLVM_COMPAT_H
