# LLVM 11-16 Legacy Array Sections Runtime Segfault - Investigation Complete

## Critical Context
**BRANCH**: `feature/minpack-lmder1-mre` (PR #8598 implementation)
**ISSUE**: Runtime segfault when using `--legacy-array-sections` with 0-based arrays
**STATUS**: Compilation works (BitCast workaround), runtime broken (descriptor fix reverted)

## Executive Summary

The branch contains PR #8598's semantic transformations but the runtime fix was reverted due to CI failures. The issue occurs when passing array elements from 0-based arrays (e.g., `A(0:*)` passing `A(1)`) to subroutines expecting assumed-size arrays (`*` dimension).

**Root Cause Identified**: The issue is that ArraySection is created BEFORE the legacy array sections code runs. During normal expression processing in SubroutineCall, `A(1)` is converted to an ArraySection, then the legacy code tries to handle it but creates unnecessary temporaries `__libasr_created__subroutine_call_*` that cause segfaults.

## Current Investigation Status (2025-09-29)

### Changes Made
1. Fixed `is_star_dimension` check to look at LAST dimension instead of first (lines 2054, 4029)
2. Added logic to set `m_start=1` for star dimensions in `process_dims` (line 2169)
3. Added early return in `legacy_promote_array_item` for UnboundedPointerArray
4. Result: Assumed-size arrays now correctly use UnboundedPointerArray type

### Remaining Issue
Despite UnboundedPointerArray being detected correctly, temporaries are STILL created because:
1. ArraySection is created during normal expression processing BEFORE legacy code runs
2. `create_ArrayRef` in `visit_FuncCallOrArray` creates ArraySection for array indexing
3. By the time `legacy_promote_array_item` is called, it receives ArraySection not ArrayItem
4. The early return for UnboundedPointerArray doesn't help because input is already transformed

### Next Steps Needed
1. Prevent ArraySection creation in `create_ArrayRef` when target is UnboundedPointerArray
2. OR: Handle ArraySection input in legacy code and avoid creating temporaries
3. OR: Completely bypass array transformation for assumed-size array arguments

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

## Key Findings from Investigation

### The Real Problem
The fundamental issue is a **calling convention mismatch** in legacy mode:
- The caller (with ArrayPhysicalCast) wants to pass array slices as pointers (FORTRAN 77 style)
- The callee (STRSM subroutine) is compiled expecting descriptors (modern Fortran style)
- Both sides need to agree on the convention

### Discovery: ArrayPhysicalCast Not Called During LLVM Codegen
- `visit_ArrayPhysicalCast` is NEVER called for SubroutineCall arguments
- Instead, ArrayPhysicalCast is handled inline in `convert_call_args` template function (line ~12387)
- This inline handling just calls `visit_expr_wrapper` which creates a descriptor

### Failed Approach: Semantic Level Fix
Attempted to use `UnboundedPointerArray` instead of `DescriptorArray` for assumed-size array parameters in legacy mode. This failed because:
- Using UnboundedPointerArray loses dimension information needed for array access
- Caused crashes during codegen due to null dimension bounds (m_dims[i].m_start was null)
- The dimension information is still needed even for assumed-size arrays

## Implementation Strategy

### Phase 1: Minimal Reproducer
```fortran
C     Minimal reproducer for legacy array sections segfault
C     Matches exact pattern from lapack_06.f90 line 78
      PROGRAM TEST
      IMPLICIT NONE
      REAL A(0:100)
      INTEGER M, N, LDA
      REAL ALPHA

      M = 6
      N = 4
      LDA = 7  ! This is M+1 from lapack_06

C     Initialize array
      A = 1.0

C     This matches lapack_06.f90 line 78:
C     CALL STRSM(..., A(1), M+1, ...)
C     where A is declared as A(0:*)
      CALL STRSM(3, N, ALPHA, A(1), LDA)

      PRINT *, 'Test completed successfully'
      END PROGRAM

      SUBROUTINE STRSM(M, N, ALPHA, A, LDA)
      IMPLICIT NONE
      INTEGER M, N, LDA
      REAL ALPHA
      REAL A(LDA, *)  ! 2D assumed-size array

C     Try to access A as a 2D array - this is where it should fail
      PRINT *, 'Accessing A(1,1) =', A(1,1)

      END SUBROUTINE
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

## Minimal Reproducer Status

The integration_tests/lapack_06.f90 has been replaced with a minimal reproducer that isolates the exact problem:
- A 0-based array A(0:100) passed as A(1) to a subroutine expecting a 2D array
- Currently segfaults with --legacy-array-sections
- Works correctly with gfortran

## Key Findings from Investigation

### The Problem Flow
1. **ASR Level**: ArrayPhysicalCast node is correctly created with:
   - `m_old = FixedSizeArray`
   - `m_new = DescriptorArray`
   - Contains ArraySection `A(1:upper)` as expected

2. **LLVM Codegen Issue**:
   - `visit_ArrayPhysicalCast` is NEVER called during LLVM codegen
   - Instead, ArrayPhysicalCast arguments are handled inline in `convert_call_args` (line ~12387)
   - The inline handling just calls `visit_expr_wrapper` which creates a descriptor
   - This descriptor is then passed to the subroutine

3. **Runtime Failure**:
   - The subroutine expects and tries to access the argument as a descriptor
   - But in legacy mode, it should be a raw pointer
   - Accessing descriptor fields on what should be a pointer causes segfault

### Why Previous Fix Attempts Failed
1. Modifying `visit_ArrayPhysicalCast` doesn't help because it's never called
2. Modifying `visit_ArrayPhysicalCastUtil` doesn't help for the same reason
3. The real handling happens in `convert_call_args` template function
4. Even when we intercept in `convert_call_args`, we're still left with a mismatch:
   - Caller wants to pass a pointer
   - Callee expects a descriptor

### The Real Problem
The fundamental issue is a **calling convention mismatch** in legacy mode:
- The caller (with ArrayPhysicalCast) wants to pass array slices as pointers (FORTRAN 77 style)
- The callee (STRSM subroutine) is compiled expecting descriptors (modern Fortran style)
- Both sides need to agree on the convention

## Proposed Solution

The fix needs to ensure consistency between caller and callee:

### Option A: Fix at Semantic Level (Recommended)
Modify the semantic analysis so that when `--legacy-array-sections` is enabled:
1. Formal array parameters in subroutines should use `UnboundedPointerArray` or `PointerArray` instead of `DescriptorArray`
2. This ensures both caller and callee agree to use raw pointers
3. Location: `ast_common_visitor.h` where formal parameters are analyzed

### Option B: Fix at Codegen Level
Handle both sides in LLVM codegen:
1. In `convert_call_args`: Pass raw pointer instead of descriptor for legacy mode
2. In subroutine generation: Accept raw pointer instead of expecting descriptor
3. More complex as it requires changes in multiple places

### Option C: Hybrid Approach
1. Keep descriptors but make them compatible
2. Create minimal descriptors that work as both pointers and descriptors
3. Risk of compatibility issues

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

## Current Status (28 Sep 2025)

### Progress Made:
1. Identified that temporary variables `__libasr_created__subroutine_call_*` are created for array sections
2. Implemented fix in LLVM codegen to pass raw pointers for dimension mismatches in legacy mode
3. Fix partially works - raw pointer is passed from caller side

### Remaining Issue:
**The callee (STRSM) is still compiled to expect a descriptor, not a raw pointer.**

When examining the LLVM IR for STRSM:
```llvm
define void @strsm(ptr %m, ptr %n, ptr %alpha, ptr %a, ptr %lda) {
  %1 = getelementptr %array, ptr %a, i32 0, i32 2  ; Accessing descriptor fields!
  ...
  ; Runtime bounds checking on the descriptor
}
```

The function expects `%a` to be a descriptor (struct with data pointer, dims, etc.) but we're passing a raw pointer.

### Root Cause:
In legacy mode with assumed-size arrays `A(LDA, *)`, the formal parameter should be compiled as:
- **UnboundedPointerArray** or raw pointer type (no descriptor access)
- No runtime bounds checking
- Direct pointer arithmetic for element access

But it's being compiled as:
- **DescriptorArray** with full descriptor structure
- Runtime bounds checking enabled
- Descriptor field access for dimensions

### The Complete Fix Requires:
1. **Caller side** (PARTIALLY DONE): Pass raw pointer instead of descriptor for legacy array sections
   - Implemented detection of `__libasr_created__subroutine_call_*` temporaries
   - Added raw pointer passing for dimension mismatches
   - Still segfaults because callee expects descriptor

2. **Callee side** (NOT DONE): Compile assumed-size array parameters as raw pointers
   - Assumed-size arrays should have `UnboundedPointerArray` physical type
   - The code already sets this in `make_Array_t_util` when `is_dimension_star` is true
   - But something is overriding it or the flag isn't being set correctly

### Where to Fix Callee Compilation:

#### Semantic Phase (ASR Generation):
In the semantic visitor when creating Variable symbols for formal parameters:
1. Check if `compiler_options.legacy_array_sections` is set
2. Check if the array dimension has `*` (assumed-size)
3. If both true, set physical type to UnboundedPointerArray instead of DescriptorArray

**Files to modify:**
- `src/lfortran/semantics/ast_symboltable_visitor.cpp` or similar
- Look for where formal parameter Variables are created
- Specifically where array types are assigned to formal parameters

#### LLVM Codegen Phase:
The LLVM codegen already handles UnboundedPointerArray correctly - it treats them as raw pointers without descriptor access. So once the ASR has the correct physical type, the LLVM codegen should work.

### Investigation Findings:

#### Code Analysis:
1. **ASR Utils** (`src/libasr/asr_utils.h`):
   - `make_Array_t_util` correctly sets `UnboundedPointerArray` when `is_dimension_star` is true (lines 32-33)
   - This should handle assumed-size arrays properly

2. **LLVM Codegen** (`src/libasr/codegen/asr_to_llvm.cpp`):
   - `visit_ArrayItem` handles UnboundedPointerArray specially (no bounds checking)
   - But formal parameters with `*` dimension are still treated as DescriptorArray

3. **Array Utils** (`src/libasr/codegen/llvm_array_utils.cpp`):
   - Bounds checking happens in `cmo_convertor_single_element` (lines 745, 790)
   - Accesses descriptor fields that don't exist for raw pointers

### Why It's Still Broken:
The assumed-size array formal parameter `A(LDA, *)` in STRSM is being compiled as DescriptorArray instead of UnboundedPointerArray. Either:
1. The `is_dimension_star` flag isn't being set during semantic analysis
2. The physical type is being overridden after initial assignment
3. Some pass is changing the array physical type

### Simpler Workaround (Not Implemented):
Disable runtime bounds checking for assumed-size arrays:
- Would need to detect UnboundedPointerArray in bounds checking code
- Skip bounds checks for these arrays
- This would prevent segfaults but is not a proper fix

## Final Status

### What Works:
- Identified root cause: calling convention mismatch between caller and callee
- Implemented partial fix for caller side (passes raw pointer for temporaries)
- Created minimal 33-line reproducer
- Comprehensive documentation of the issue

### What's Still Broken:
- Callee side still expects descriptors for assumed-size arrays
- Runtime segfault when accessing array as 2D in STRSM
- The `is_dimension_star` logic exists but isn't being applied correctly

### Next Steps:
1. Debug why assumed-size formal parameters aren't getting `UnboundedPointerArray` type
2. Trace where `is_dimension_star` is set and used
3. Check if any ASR passes override the physical type
4. Ensure bounds checking is skipped for UnboundedPointerArray

## Key Insight

The infrastructure for handling assumed-size arrays (UnboundedPointerArray) already exists in LFortran. The issue is that it's not being activated for formal parameters with `*` dimension. This suggests the fix might be simpler than initially thought - we just need to ensure the existing code path is triggered correctly.

This document contains all information needed to implement the fix from scratch without any prior context.