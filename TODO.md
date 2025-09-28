# LLVM 11-16 Compatibility Fix for LAPACK Sequence Association

## Executive Summary
**PROBLEM**: Runtime segfault in lapack_06.f90 with `--legacy-array-sections` flag
**BRANCH**: feature/minpack-lmder1-mre (based on PR #8598)
**STATUS**: Compilation works, runtime segfaults persist
**ROOT CAUSE**: UnboundedPointerArray descriptor creation was reverted (commit dbceb9625)

## Current Branch State
This branch IS PR #8598 with the following status:
1. ✅ **Semantic transformations** - PRESENT (commit 5865fbb32)
2. ✅ **BitCast workaround** - PRESENT (commit 5fb43d3f7)
3. ❌ **Runtime fix** - ATTEMPTED but REVERTED (commits 5e5a85355 → dbceb9625)

## Problem Analysis

### What --legacy-array-sections Does
The flag enables FORTRAN 77 sequence association for legacy code compatibility:

1. **Semantic Phase** (`ast_common_visitor.h:6737-6951`):
   - `legacy_array_sections_helper()` - Entry point, checks flag and processes arguments
   - `legacy_promote_array_item()` - Converts ArrayItem → ArraySection
   - `legacy_promote_array_expr()` - Handles non-ArrayItem array expressions
   - `legacy_resolve_formal_dims()` - Resolves formal parameter dimensions
   - Example: `call sub(a(1,j))` → transforms `a(1,j)` to `a(1:16,j)` if formal expects `a(16)`

2. **Codegen Phase** (`asr_to_llvm.cpp`):
   - `visit_ArraySection()` (line 3141) - Creates array section descriptors
   - `create_array_section_descriptor()` (line 2984) - New descriptor creation
   - `ArrayPhysicalCastUtil()` (line 7928) - Handles physical type conversions
   - `handle_array_section_association_to_pointer()` (line 6582) - Pointer association

3. **Pass Management** (`promote_allocatable_to_nonallocatable.cpp:395`):
   - Skips promotion when `legacy_array_sections` is active

### The Segfault Issue

**Location**: `lapack_06.f90` line 64-80
```fortran
CALL STRSM( 'L', 'L', 'N', DIAG, M1, N, ALPHA, A( 0 ), M, B, LDB )
CALL SGEMM( 'N', 'N', M2, N, M1, -ONE, A( M1 ), M, B, LDB, ALPHA, B( M1, 0 ), LDB )
CALL STRSM( 'L', 'L', 'N', DIAG, K, N, ALPHA, A( 1 ), M+1, B, LDB )
CALL SGEMM( 'N', 'N', K, N, K, -ONE, A( K+1 ), M+1, B, LDB, ALPHA, B( K, 0 ), LDB )
```

**Problem Patterns**:
- `A(0)` - 0-based array passed as pointer
- `A(M1)`, `A(K+1)` - Offset array elements
- `B(M1, 0)`, `B(K, 0)` - 2D arrays with 0-based indexing

**Root Cause**: When passing `A(K+1)` from a 0-based array `A(0:*)` to a subroutine expecting an array, the UnboundedPointerArray descriptor is not properly created, leading to NULL pointer dereference.

## What Has NOT Worked

### 1. BitCast Workaround Alone (commit 5fb43d3f7)
- **What**: Added BitCast in `CreateGEP2` and `CreateInBoundsGEPDeprecated` for LLVM ≤16
- **Result**: ✅ Fixes compilation, ❌ Runtime still segfaults
- **Why Failed**: Only masks type mismatches, doesn't fix descriptor creation

### 2. Complex ArrayPhysicalCastUtil Fix (commit 5e5a85355, reverted in dbceb9625)
- **What**: Added full descriptor creation for FixedSizeArray → UnboundedPointerArray
- **Implementation**:
  ```cpp
  // Created descriptor with dimension info
  llvm::Type* target_type = arr_descr->get_array_type(...);
  llvm::AllocaInst *target = llvm_utils->CreateAlloca(target_type, ...);
  // Set data pointer, offset, dimensions, rank
  ```
- **Result**: ❌ Caused CI failures - "array shape mismatch errors"
- **Why Failed**: Likely incompatible with existing array handling elsewhere

### 3. Semantic Transformation Alone (commit 5865fbb32)
- **What**: Transform `a(1,j)` → `a(1:16,j)` in semantic phase
- **Result**: ✅ ASR looks correct, ❌ Runtime still segfaults
- **Why Failed**: The transformation happens but descriptor isn't created properly in codegen

## What COULD Work - Clean Options

### Option 1: Minimal C-Style Pointer Approach (RECOMMENDED)
**Rationale**: Legacy array sections should behave like C pointers, not Fortran descriptors

**Implementation**:
```cpp
// In ArrayPhysicalCastUtil for UnboundedPointerArray case:
if (compiler_options.legacy_array_sections &&
    m_old == ASR::array_physical_typeType::FixedSizeArray &&
    m_new == ASR::array_physical_typeType::UnboundedPointerArray) {
    // Just pass raw pointer, no descriptor
    if (needs_offset) {
        tmp = llvm_utils->create_gep2(arr_type, tmp, offset_value);
    }
    tmp = ensure_pointer_type(tmp);
    // Don't create descriptor, just pass pointer
    return;
}
```

**Guards**:
- Check `compiler_options.legacy_array_sections`
- Check physical type conversion
- LLVM version doesn't matter for this approach

### Option 2: Simple Descriptor with Offset Only
**Rationale**: Create minimal descriptor that only tracks offset, not dimensions

**Implementation**:
```cpp
// Create lightweight descriptor with just data pointer and offset
struct LegacyArrayDescriptor {
    void* data;
    int32_t offset;
};
```

**Guards**:
- `#if LLVM_VERSION_MAJOR <= 16` for typed pointers
- `compiler_options.legacy_array_sections` check

### Option 3: Fix at ASR Level
**Rationale**: Change array physical type earlier in semantic phase

**Implementation**:
- In `legacy_promote_array_item()`, set physical type to `PointerArray` not `UnboundedPointerArray`
- This avoids the problematic conversion entirely

### Option 4: Bypass Descriptor for Legacy Mode
**Rationale**: Legacy FORTRAN 77 didn't have descriptors

**Implementation**:
- Add new array physical type: `LegacyPointer`
- Skip all descriptor creation for this type
- Pass raw pointers directly

## Test Commands

### Build & Test with LLVM 11
```bash
eval "$(micromamba shell hook --shell bash)"
micromamba create -n lf-llvm11 -f ci/environment.yml llvm=11 -c conda-forge -y
micromamba activate lf-llvm11

cmake -S . -B build-llvm11 -G Ninja \
    -DCMAKE_BUILD_TYPE=Debug \
    -DWITH_LLVM=ON \
    -DLLVM_VERSION=11 \
    -DWITH_RUNTIME_STACKTRACE=yes
cmake --build build-llvm11 -j

# Test reproducer
build-llvm11/src/bin/lfortran integration_tests/lapack_06.f90 \
    --fixed-form --implicit-interface --legacy-array-sections \
    -o test_lapack_06_llvm11
./test_lapack_06_llvm11  # EXPECTED: Segfault currently

# Test with LLVM IR output
build-llvm11/src/bin/lfortran integration_tests/lapack_06.f90 \
    --fixed-form --implicit-interface --legacy-array-sections \
    --show-llvm > lapack_06_llvm11.ll
```

### Build & Test with LLVM 21
```bash
micromamba create -n lf-llvm21 -f ci/environment.yml llvm=21 -c conda-forge -y
micromamba activate lf-llvm21

cmake -S . -B build-llvm21 -G Ninja \
    -DCMAKE_BUILD_TYPE=Debug \
    -DWITH_LLVM=ON \
    -DLLVM_VERSION=21
cmake --build build-llvm21 -j

build-llvm21/src/bin/lfortran integration_tests/lapack_06.f90 \
    --fixed-form --implicit-interface --legacy-array-sections \
    -o test_lapack_06_llvm21
./test_lapack_06_llvm21  # EXPECTED: Segfault currently
```

### Debug with GDB
```bash
gdb ./test_lapack_06_llvm11
(gdb) run
(gdb) bt  # Show backtrace at segfault
(gdb) frame 1  # Go to STFSM frame
(gdb) info locals  # Show local variables
```

### Third-Party Tests
```bash
# Modern Minpack (uses legacy-array-sections)
git clone https://github.com/fortran-lang/minpack
cd minpack && git checkout c0b5aea9fcd2b83865af921a7a7e881904f8d3c2
$FC src/minpack.f90 -c --legacy-array-sections
$FC examples/example_lmder1.f90 --legacy-array-sections
./a.out  # Will likely segfault with same issue

# Reference LAPACK
git clone https://github.com/Reference-LAPACK/lapack.git
cd lapack && git checkout v3.12.0
# Apply patch from ci/test_third_party_codes.sh:882-898
mkdir build && cd build
cmake -DCMAKE_Fortran_COMPILER=$FC \
      -DCMAKE_Fortran_FLAGS="--cpp --fixed-form-infer --implicit-interface --legacy-array-sections" \
      -DBUILD_TESTING=OFF ..
make -j8  # Will compile but runtime issues expected
```

## Implementation Plan

### Step 1: Create Minimal Reproducer (PRIORITY)
```fortran
! minimal_segfault.f90
PROGRAM TEST
    REAL A(0:10), B(5)
    A = 1.0
    CALL SUB(A(1), 5)  ! Pass A starting at index 1
CONTAINS
    SUBROUTINE SUB(X, N)
        REAL X(N)
        PRINT *, X(1)  ! Should print 1.0, but segfaults
    END SUBROUTINE
END PROGRAM
```

### Step 2: Implement Option 1 (C-Style Pointer)
1. Modify `ArrayPhysicalCastUtil` in `asr_to_llvm.cpp:7986`
2. Add guard for `legacy_array_sections`
3. Skip descriptor creation, pass raw pointer with offset

### Step 3: Test Thoroughly
1. Test minimal reproducer
2. Test lapack_06.f90
3. Run integration test suite
4. Test third-party codes

## Key Code Locations

### Semantic Phase
- `src/lfortran/semantics/ast_common_visitor.h:6737-6951` - Legacy array transformations
- Lines 6845-6858: The incomplete fix attempt for sequence association

### Codegen Phase
- `src/libasr/codegen/asr_to_llvm.cpp:7986-7995` - ArrayPhysicalCastUtil (REVERTED)
- `src/libasr/codegen/asr_to_llvm.cpp:2984-3138` - create_array_section_descriptor
- `src/libasr/codegen/asr_to_llvm.cpp:3141-3184` - visit_ArraySection
- `src/libasr/codegen/llvm_utils.cpp:1624-1664` - CreateGEP functions with BitCast

### Array Descriptors
- `src/libasr/codegen/llvm_array_utils.cpp:719-758` - cmo_convertor_single_element
- `src/libasr/codegen/llvm_array_utils.cpp:95-100` - SimpleCMODescriptor constructor

## Conclusion

The current implementation has all the semantic machinery but lacks proper runtime support. The reverted ArrayPhysicalCastUtil fix was too complex and broke other tests.

**Recommendation**: Implement Option 1 (C-Style Pointer) as it aligns with the original FORTRAN 77 behavior where arrays were just pointers without descriptors. This is simpler, less likely to break existing code, and matches the intent of `--legacy-array-sections`.