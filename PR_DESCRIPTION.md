# Fix EQUIVALENCE+DATA Array Shape Descriptor Construction

## Summary
Fixes shape mismatch runtime errors when EQUIVALENCE and DATA statements interact with multi-dimensional arrays.

Addresses part of issue #9134.

## Problem
The CPtrToPointer shape array descriptor only stored the first dimension extent (hardcoded length=1) instead of all array rank dimensions. This caused runtime errors:
```
Array shape mismatch in assignment to 'dt19xa':
LHS should have size 8, but got 1 instead
```

## Solution
1. Set shape array dimension length to array rank (not hardcoded to 1)
2. Change shape array physical type from PointerArray to FixedSizeArray
3. Skip compile-time shape validation for descriptor arrays (shape set at runtime by CPtrToPointer)
4. Register equivalence_12.f90 in integration test suite

## Changes
- [ast_common_visitor.h](src/lfortran/semantics/ast_common_visitor.h): Fix shape array dimension construction
- [asr_to_llvm.cpp](src/libasr/codegen/asr_to_llvm.cpp): Skip bounds check for descriptor arrays
- [CMakeLists.txt](integration_tests/CMakeLists.txt): Register equivalence_12.f90 test

## Test Results

### PASS ✓
- **equivalence_10.f90**: 2D arrays (26,4) with EQUIVALENCE+DATA

### Shape Fixed, New Backend Issue
- **equivalence_11.f90**: Shape mismatch resolved, but abs() intrinsic codegen fails
- **equivalence_12.f90**: Shape mismatch resolved, but abs() intrinsic codegen fails

The abs() intrinsic failure appears to be a separate backend issue when operating on descriptor array elements. Tests compile successfully with GFortran.

## Verification
```bash
# Build
make -j4

# Test (shape issue fixed)
./src/bin/lfortran integration_tests/equivalence_10.f90  # PASS
./src/bin/lfortran integration_tests/equivalence_11.f90  # Shape OK, abs() fails
./src/bin/lfortran integration_tests/equivalence_12.f90  # Shape OK, abs() fails
```

## Known Limitation
The abs() intrinsic codegen fails when the argument is an array element from an equivalenced descriptor array. This is a separate issue that needs investigation in the intrinsic function passes.
