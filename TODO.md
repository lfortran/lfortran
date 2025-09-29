# LAPACK --legacy-array-sections Implementation

## Status: ✅ COMPLETE (5/5 tests passing - 100%!)

### Reference-LAPACK v3.12.0 Build Success! 🎉

Successfully built the official Reference-LAPACK v3.12.0 with LFortran using `--legacy-array-sections`:
- **BLAS library**: 1.1M (`libblas.a`)
- **LAPACK library**: 34M (`liblapack.a`)
- **Build errors**: 0
- **Style suggestions**: 6 (non-blocking)
- **Compilation flags**: `--cpp --fixed-form-infer --implicit-interface --legacy-array-sections`

### Implementation

Clean solution using BindC ABI + UnboundedPointerArray:

1. **AST-to-ASR** (`ast_common_visitor.h`):
   - Mark functions as BindC ABI when `--legacy-array-sections` is enabled
   - Convert array parameters to UnboundedPointerArray physical type
   - Use `duplicate_type` to avoid shared type conflicts

2. **LLVM Codegen** (`asr_to_llvm.cpp`):
   - Extract raw data pointers for BindC array arguments
   - Handle UnboundedPointerArray with assumed-size dimensions (null m_start)
   - Disable bounds checking for UnboundedPointerArray arrays

3. **ASR Verification** (`asr_verify.cpp`):
   - Skip physical type mismatch checks when UnboundedPointerArray is involved
   - Allows FORTRAN 77 style type flexibility

### Test Results

| Test | Pattern | Status |
|------|---------|--------|
| lapack_02 | Scalar/array ambiguity (SLASCL) | ✅ PASS |
| lapack_03 | Recursive calls with sequence association (STRMM) | ✅ PASS |
| lapack_04 | EXTERNAL functions with varied argument patterns | ✅ PASS |
| lapack_05 | 0-based array with assumed-size formal (STRSM) | ✅ PASS |
| minpack_04 | Minpack lmder1 with legacy sections | ✅ PASS |

### What Works

✅ Functions defined in the current file (with bodies)
✅ EXTERNAL functions defined later in the same file
✅ Assumed-size arrays (`*`) in formal parameters
✅ 0-based arrays passed to 1-based formal parameters
✅ 1D arrays passed where 2D expected (sequence association)
✅ Array elements passed as array starts (e.g., `A(1)` → 2D formal)
✅ Mixed scalar/array calls to the same function
✅ BindC-style raw pointer passing
✅ FORTRAN 77 compatibility patterns

### Files Modified

1. `src/lfortran/semantics/ast_common_visitor.h` - Mark BindC ABI + convert types
2. `src/libasr/codegen/asr_to_llvm.cpp` - Handle UnboundedPointerArray in codegen
3. `src/libasr/codegen/llvm_array_utils.cpp` - Handle assumed-size dims (null m_start)
4. `src/libasr/asr_verify.cpp` - Skip verification for UnboundedPointerArray
5. `integration_tests/lapack_*.f90` - Test cases
6. `integration_tests/minpack_04.f90` - Test case
7. `integration_tests/CMakeLists.txt` - Register tests

### Usage

```bash
lfortran --implicit-interface --legacy-array-sections file.f90
```

For fixed-form FORTRAN 77:
```bash
lfortran --fixed-form --implicit-interface --legacy-array-sections file.f
```

### Technical Details

- **UnboundedPointerArray**: Physical type marker for EXTERNAL function parameters with legacy array sections
- **BindC ABI**: Signals raw pointer passing (no descriptors) at call sites
- **Selective conversion**: Only convert parameters to UnboundedPointerArray for functions WITHOUT bodies (`n_body == 0`)
  - Functions with bodies keep descriptor-based arrays to support array sections within their implementation
  - EXTERNAL function signatures get converted to enable raw pointer passing at call sites
- **Assumed-size handling**: `m_start == nullptr` for `*` dimensions → default to lbound=1
- **ASR verification**: Skip physical type mismatch checks when UnboundedPointerArray is involved
- **Type duplication**: Uses `duplicate_type` to avoid shared type object conflicts