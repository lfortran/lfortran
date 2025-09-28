# LLVM 11-16 Legacy Array Sections Runtime Segfault - Complete Analysis

## Critical Context
**BRANCH**: `feature/minpack-lmder1-mre` (PR #8598 implementation)
**ISSUE**: Runtime segfault when using `--legacy-array-sections` with 0-based arrays
**STATUS**: Compilation works (BitCast workaround), runtime broken (descriptor fix reverted)

## Executive Summary

The branch contains PR #8598's semantic transformations but the runtime fix was reverted due to CI failures. The issue occurs when passing array elements from 0-based arrays (e.g., `A(0:*)` passing `A(1)`) to subroutines expecting arrays. The transformation `A(1)` → `A(1:16)` happens correctly in the semantic phase, but the LLVM codegen fails to create proper descriptors for `UnboundedPointerArray` type, causing NULL pointer dereference at runtime.

## Complete Git History

### Current Branch Commits (newest to oldest)
```
dbceb9625 fix: revert complex ArrayPhysicalCastUtil implementation causing CI failures
5e5a85355 fix: implement complete array descriptor creation for UnboundedPointerArray
5fb43d3f7 fix: restore BitCast workaround for LLVM 11-16 with legacy array sections
eb6a91e09 test: add lapack_06 reproducer for LLVM 11-16 GEP assertion failure
5865fbb32 fix: implement proper LAPACK sequence association in semantic analysis
```

### What Each Commit Did

#### 5865fbb32 - Semantic Transformations (KEPT)
Added in `ast_common_visitor.h:6737-6951`:
- `legacy_array_sections_helper()` - Entry point, checks flag
- `legacy_promote_array_item()` - Converts ArrayItem → ArraySection
- `legacy_promote_array_expr()` - Handles non-ArrayItem arrays
- `legacy_resolve_formal_dims()` - Resolves formal parameter dimensions

#### 5fb43d3f7 - BitCast Workaround (KEPT)
Added in `llvm_utils.cpp:1624-1664`:
```cpp
#if LLVM_VERSION_MAJOR <= 16
    if (compiler_options.legacy_array_sections) {
        x = builder->CreateBitCast(x, t->getPointerTo());
    }
#endif
```

#### 5e5a85355 - Descriptor Creation (ATTEMPTED)
Added in `asr_to_llvm.cpp:7986-8078` (88 lines):
```cpp
// Created full descriptor with:
llvm::Type* target_type = arr_descr->get_array_type(...);
llvm::AllocaInst *target = llvm_utils->CreateAlloca(target_type, ...);
// Set data pointer, offset, dimensions array, rank
// For each dimension: stride, lower_bound, size
```

#### dbceb9625 - Revert (CURRENT STATE)
Reverted back to simple 2-line implementation:
```cpp
if (...) {
    tmp = llvm_utils->create_gep2(arr_type, tmp, 0);
}
tmp = ensure_pointer_type(tmp);
```

## Array Physical Types (ASR.asdl:242)

```
array_physical_type =
    DescriptorArray           | // Full Fortran descriptor with bounds
    PointerArray              | // Pointer with compile-time known bounds
    UnboundedPointerArray     | // Pointer with runtime bounds (PROBLEMATIC)
    FixedSizeArray           | // Stack-allocated fixed-size array
    StringArraySinglePointer  | // Special case for character arrays
    NumPyArray               | // NumPy interop
    ISODescriptorArray       | // ISO C descriptor
    SIMDArray                | // SIMD optimized arrays
```

## Complete Code Flow

### 1. Fortran Source → AST
```fortran
CALL STRSM('L', 'L', 'N', DIAG, K, N, ALPHA, A(1), M+1, B, LDB)
```

### 2. AST → ASR (Semantic Analysis)
**Location**: `ast_common_visitor.h:6953-6981`

With `--legacy-array-sections`:
1. `legacy_array_sections_helper()` is called
2. Detects `A(1)` is ArrayItem, formal expects array
3. Calls `legacy_promote_array_item()` (line 6771)
4. Creates ArraySection: `A(1)` → `A(1:formal_upper_bound)`
5. If formal is UnboundedPointerArray, creates ArrayPhysicalCast

**Key code** (lines 6844-6858):
```cpp
if (formal_phy != ASR::array_physical_typeType::PointerArray &&
    formal_phy != ASR::array_physical_typeType::UnboundedPointerArray) {
    // Handle sequence association for regular arrays (LAPACK pattern)
    if (formal_array->n_dims >= 1 && indices.size() >= 1) {
        ASR::dimension_t &first_formal_dim = formal_array->m_dims[0];
        if (first_formal_dim.m_length) {
            indices.p[0].m_right = first_formal_dim.m_length;
            ASR::expr_t *corrected_section = ASRUtils::EXPR(ASR::make_ArraySection_t(
                al, loc, array_expr, indices.p, indices.size(), section_type, nullptr));
            return corrected_section;
        }
    }
}
```

### 3. ASR → LLVM IR (Code Generation)
**Problem Location**: `asr_to_llvm.cpp:7986-7995`

#### Current (Broken) Implementation:
```cpp
} else if(
    m_new == ASR::array_physical_typeType::UnboundedPointerArray &&
    m_old == ASR::array_physical_typeType::FixedSizeArray) {
    if( ((ASRUtils::expr_value(m_arg) &&
        !ASR::is_a<ASR::ArrayConstant_t>(*ASRUtils::expr_value(m_arg))) ||
        ASRUtils::expr_value(m_arg) == nullptr) &&
        !ASR::is_a<ASR::ArrayConstructor_t>(*m_arg) ) {
        tmp = llvm_utils->create_gep2(arr_type, tmp, 0);
    }
    tmp = ensure_pointer_type(tmp);
}
```

**Issue**: Just passes a raw pointer without descriptor information.

#### What Was Attempted (5e5a85355):
Created full descriptor with:
1. Data pointer
2. Offset (0)
3. Dimension descriptors array
4. For each dimension: stride, lower_bound, size
5. Rank

**Why It Failed**: CI showed "array shape mismatch errors" - likely incompatible with how descriptors are expected elsewhere in the code.

### 4. LLVM IR → Machine Code
The LLVM IR shows calls like:
```llvm
call void @strsm(ptr @string_const.8, ptr @string_const.10,
                 ptr @string_const.12, ptr %diag, ptr %m1,
                 ptr %n, ptr %alpha, ptr %a, ptr %m,
                 ptr %145, ptr %ldb)
```
Where `%145` is just a pointer without proper descriptor, causing segfault when STRSM tries to access it as an array.

## The Segfault Mechanism

### lapack_06.f90 Critical Lines:
```fortran
64: CALL STRSM( 'L', 'L', 'N', DIAG, M1, N, ALPHA, A( 0 ), M, B, LDB )
79: CALL STRSM( 'L', 'L', 'N', DIAG, K, N, ALPHA, A( 1 ), M+1, B, LDB )
```

### What Happens:
1. `A` is declared as `A(0:*)` - 0-based assumed-size array
2. `A(1)` is transformed to ArraySection `A(1:upper)` in ASR
3. ArrayPhysicalCast from FixedSizeArray to UnboundedPointerArray
4. In LLVM codegen, just passes `&A[1]` as raw pointer
5. STRSM expects a descriptor and dereferences NULL fields
6. **SEGFAULT** at first array access

### GDB Analysis:
```bash
Program received signal SIGSEGV, Segmentation fault.
0x00005555555557e6 in stfsm ()
```

## SimpleCMODescriptor Structure

**Location**: `llvm_array_utils.cpp:95-100`

The descriptor structure (SimpleCMODescriptor) expects:
```cpp
struct {
    void* data;           // Pointer to array data
    int32_t offset;       // Offset for 0-based arrays
    dim_descriptor* dims; // Pointer to dimension descriptors
    int32_t rank;        // Number of dimensions
}

struct dim_descriptor {
    int32_t stride;      // Stride for this dimension
    int32_t lower_bound; // Lower bound
    int32_t size;        // Size (not upper bound!)
}
```

**Key Methods**:
- `get_pointer_to_data()` - Returns pointer to data field
- `get_offset()` - Returns offset field
- `get_pointer_to_dimension_descriptor_array()` - Returns dims field
- `get_rank()` - Returns rank field
- `cmo_convertor_single_element()` - Calculates element address

## Why Current Approaches Failed

### 1. BitCast Alone (Still Present)
- **Location**: `llvm_utils.cpp:1630-1631, 1653-1654`
- **Effect**: Fixes LLVM ≤16 compilation by masking type mismatches
- **Problem**: Doesn't create descriptors, just masks the type issue

### 2. Complex Descriptor Creation (Reverted)
- **Location**: `asr_to_llvm.cpp:7986-8078` (was 88 lines)
- **Effect**: Created full descriptor with all fields
- **Problem**: CI failures - "array shape mismatch errors"
- **Root Cause**: Likely incompatible with existing descriptor expectations

### 3. Semantic Transformation Alone
- **Location**: `ast_common_visitor.h:6844-6858`
- **Effect**: Correctly transforms ArrayItem → ArraySection in ASR
- **Problem**: Codegen doesn't handle UnboundedPointerArray properly

## Clean Solution Options

### Option 1: Direct Pointer Pass (RECOMMENDED)
**Rationale**: FORTRAN 77 didn't have descriptors, just passed pointers

```cpp
// In ArrayPhysicalCastUtil at line 7986:
} else if(
    m_new == ASR::array_physical_typeType::UnboundedPointerArray &&
    m_old == ASR::array_physical_typeType::FixedSizeArray) {

    if (compiler_options.legacy_array_sections) {
        // For legacy mode, just pass pointer with offset
        // This matches FORTRAN 77 behavior

        // Calculate offset if needed (e.g., for A(K+1))
        llvm::Value* offset = /* calculate from ArraySection info */;
        if (offset) {
            tmp = llvm_utils->create_ptr_gep2(arr_type, tmp, offset);
        }
        tmp = ensure_pointer_type(tmp);
        // DON'T create descriptor, just return pointer
        return;
    }

    // Non-legacy path (current broken code)
    ...
}
```

### Option 2: Minimal Descriptor
**Rationale**: Create descriptor with only data pointer, no dimension info

```cpp
// Create minimal descriptor
llvm::Type* min_desc_type = llvm::StructType::get(context, {
    llvm::Type::getInt8Ty(context)->getPointerTo(), // data
    llvm::Type::getInt32Ty(context)                 // offset
});
llvm::Value* desc = llvm_utils->CreateAlloca(min_desc_type, ...);
// Store data pointer and offset only
```

### Option 3: Use PointerArray Instead
**Rationale**: PointerArray works, UnboundedPointerArray doesn't

In `legacy_promote_array_item()` line 6840:
```cpp
// Change this:
if (formal_phy != ASR::array_physical_typeType::PointerArray &&
    formal_phy != ASR::array_physical_typeType::UnboundedPointerArray) {
// To this:
if (formal_phy != ASR::array_physical_typeType::PointerArray) {
    // Force PointerArray for legacy mode
    formal_phy = ASR::array_physical_typeType::PointerArray;
```

### Option 4: Fix Descriptor Properly
**Rationale**: Make the descriptor compatible with what's expected

The issue with the attempted fix was likely:
1. Wrong descriptor structure type
2. Incorrect field initialization order
3. Missing alignment requirements
4. Incompatible with SimpleCMODescriptor expectations

## Implementation Strategy

### Phase 1: Minimal Reproducer
```fortran
! test_minimal.f90
PROGRAM TEST
    REAL A(0:10)
    A = 1.0
    CALL SUB(A(1), 10)  ! Pass from index 1
CONTAINS
    SUBROUTINE SUB(X, N)
        INTEGER N
        REAL X(N)
        PRINT *, X(1)  ! Should print 1.0
    END SUBROUTINE
END PROGRAM
```

Test commands:
```bash
# Compile and run
build/src/bin/lfortran test_minimal.f90 --legacy-array-sections -o test_minimal
./test_minimal  # Currently segfaults

# Debug
gdb ./test_minimal
run
bt
frame 1
info locals
```

### Phase 2: Implement Option 1 (Direct Pointer)

1. **Modify ArrayPhysicalCastUtil** (`asr_to_llvm.cpp:7986`):
```cpp
} else if(
    m_new == ASR::array_physical_typeType::UnboundedPointerArray &&
    m_old == ASR::array_physical_typeType::FixedSizeArray) {

    // NEW CODE START
    if (compiler_options.legacy_array_sections) {
        // For legacy FORTRAN 77, pass raw pointer
        // Extract offset from ArraySection if present
        ASR::ArraySection_t* array_section = nullptr;
        if (ASR::is_a<ASR::ArraySection_t>(*m_arg)) {
            array_section = ASR::down_cast<ASR::ArraySection_t>(m_arg);
            // Calculate offset from first index
            if (array_section->n_args > 0 && array_section->m_args[0].m_left) {
                visit_expr_wrapper(array_section->m_args[0].m_left, true);
                llvm::Value* offset = tmp;
                tmp = arg; // Restore array pointer
                tmp = llvm_utils->create_ptr_gep2(arr_type, tmp, offset);
            }
        } else {
            // Simple case - just pass pointer
            if( ((ASRUtils::expr_value(m_arg) &&
                !ASR::is_a<ASR::ArrayConstant_t>(*ASRUtils::expr_value(m_arg))) ||
                ASRUtils::expr_value(m_arg) == nullptr) &&
                !ASR::is_a<ASR::ArrayConstructor_t>(*m_arg) ) {
                tmp = llvm_utils->create_gep2(arr_type, tmp, 0);
            }
        }
        tmp = ensure_pointer_type(tmp);
        return; // Don't create descriptor
    }
    // NEW CODE END

    // Original code for non-legacy mode
    if( ((ASRUtils::expr_value(m_arg) &&
        !ASR::is_a<ASR::ArrayConstant_t>(*ASRUtils::expr_value(m_arg))) ||
        ASRUtils::expr_value(m_arg) == nullptr) &&
        !ASR::is_a<ASR::ArrayConstructor_t>(*m_arg) ) {
        tmp = llvm_utils->create_gep2(arr_type, tmp, 0);
    }
    tmp = ensure_pointer_type(tmp);
}
```

2. **Guards**:
   - `compiler_options.legacy_array_sections` check
   - LLVM version agnostic (works for all versions)
   - Only affects UnboundedPointerArray conversion

### Phase 3: Testing

#### Unit Test:
```bash
# Test minimal reproducer
build/src/bin/lfortran test_minimal.f90 --legacy-array-sections -o test_minimal
./test_minimal  # Should print 1.0

# Test lapack_06
build/src/bin/lfortran integration_tests/lapack_06.f90 \
    --fixed-form --implicit-interface --legacy-array-sections \
    -o test_lapack_06
./test_lapack_06  # Should print "All tests completed successfully"
```

#### Integration Tests:
```bash
cd integration_tests
./run_tests.py -b llvm -t lapack_06
cd ..
```

#### Third-Party Tests:
```bash
# Minpack
FC=build/src/bin/lfortran
git clone https://github.com/fortran-lang/minpack && cd minpack
git checkout c0b5aea9fcd2b83865af921a7a7e881904f8d3c2
$FC src/minpack.f90 -c --legacy-array-sections
$FC examples/example_lmder1.f90 --legacy-array-sections
./a.out  # Should run successfully

# LAPACK
git clone https://github.com/Reference-LAPACK/lapack.git && cd lapack
git checkout v3.12.0
mkdir build && cd build
cmake -DCMAKE_Fortran_COMPILER=$FC \
      -DCMAKE_Fortran_FLAGS="--cpp --fixed-form-infer --implicit-interface --legacy-array-sections" \
      -DBUILD_TESTING=OFF ..
make -j8
```

## Complete Build Instructions

### LLVM 11 Environment:
```bash
# Setup
eval "$(micromamba shell hook --shell bash)"
micromamba create -n lf-llvm11 -f ci/environment.yml llvm=11 -c conda-forge -y
micromamba activate lf-llvm11

# Build
cmake -S . -B build-llvm11 -G Ninja \
    -DCMAKE_BUILD_TYPE=Debug \
    -DWITH_LLVM=ON \
    -DLLVM_VERSION=11 \
    -DWITH_RUNTIME_STACKTRACE=yes
cmake --build build-llvm11 -j

# Test
build-llvm11/src/bin/lfortran integration_tests/lapack_06.f90 \
    --fixed-form --implicit-interface --legacy-array-sections \
    -o test_lapack_06_llvm11
./test_lapack_06_llvm11
```

### LLVM 21 Environment:
```bash
# Setup
micromamba create -n lf-llvm21 -f ci/environment.yml llvm=21 -c conda-forge -y
micromamba activate lf-llvm21

# Build
cmake -S . -B build-llvm21 -G Ninja \
    -DCMAKE_BUILD_TYPE=Debug \
    -DWITH_LLVM=ON \
    -DLLVM_VERSION=21
cmake --build build-llvm21 -j

# Test
build-llvm21/src/bin/lfortran integration_tests/lapack_06.f90 \
    --fixed-form --implicit-interface --legacy-array-sections \
    -o test_lapack_06_llvm21
./test_lapack_06_llvm21
```

## Key File Locations Reference

### Semantic Phase:
- `src/lfortran/semantics/ast_common_visitor.h`
  - Lines 6737-6951: Legacy array sections implementation
  - Line 6771: `legacy_promote_array_item()` entry
  - Lines 6844-6858: Sequence association handling

### Codegen Phase:
- `src/libasr/codegen/asr_to_llvm.cpp`
  - Lines 7928-8094: `ArrayPhysicalCastUtil()` function
  - Lines 7986-7995: UnboundedPointerArray conversion (BROKEN)
  - Lines 2984-3138: `create_array_section_descriptor()`
  - Lines 3141-3184: `visit_ArraySection()`
  - Lines 6582-6650: `handle_array_section_association_to_pointer()`

- `src/libasr/codegen/llvm_utils.cpp`
  - Lines 1624-1664: CreateGEP functions with BitCast workaround

### Array Descriptors:
- `src/libasr/codegen/llvm_array_utils.cpp`
  - Lines 95-100: SimpleCMODescriptor constructor
  - Lines 719-758: `cmo_convertor_single_element()`
  - Lines 680-691: `get_offset()`

### ASR Definitions:
- `src/libasr/ASR.asdl`
  - Line 219: Array ttype definition
  - Line 242: array_physical_type enum

### Pass Management:
- `src/libasr/pass/promote_allocatable_to_nonallocatable.cpp`
  - Line 395: Skip when legacy_array_sections is active

## Verification Checklist

- [ ] Minimal reproducer compiles and runs
- [ ] lapack_06.f90 compiles and runs
- [ ] Integration test suite passes
- [ ] Minpack example_lmder1 runs
- [ ] LAPACK builds successfully
- [ ] No regression in non-legacy mode
- [ ] Works with LLVM 11-16 (typed pointers)
- [ ] Works with LLVM 17+ (opaque pointers)

## Final Notes

The issue is well-understood: UnboundedPointerArray expects a descriptor but legacy array sections should just pass raw pointers like FORTRAN 77. The semantic transformations are correct, but the codegen needs to be fixed to not create descriptors for legacy mode.

The recommended fix (Option 1) is simple, clean, and aligns with the original FORTRAN 77 behavior that `--legacy-array-sections` is meant to emulate. It avoids the complexity of descriptors entirely for legacy code.

This document contains all information needed to implement the fix from scratch without any prior context.