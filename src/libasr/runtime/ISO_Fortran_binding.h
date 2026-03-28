/*
 * LFortran's ISO_Fortran_binding.h
 *
 * Implements the C descriptor types defined in Fortran 2018 subclause 18.5.
 * The struct layout matches LFortran's internal array descriptor as generated
 * by the LLVM codegen (see llvm_array_utils.cpp).
 *
 * Each Fortran compiler ships its own version of this header because the
 * standard leaves the descriptor layout implementation-defined.
 */

#ifndef CFI_ISO_FORTRAN_BINDING_H_
#define CFI_ISO_FORTRAN_BINDING_H_

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#define CFI_VERSION 20260322
#define CFI_MAX_RANK 15

typedef unsigned char CFI_rank_t;
typedef ptrdiff_t CFI_index_t;
typedef unsigned char CFI_attribute_t;
typedef int8_t CFI_type_t;

#define CFI_attribute_pointer 1
#define CFI_attribute_allocatable 2
#define CFI_attribute_other 0

/* Type codes */
#define CFI_type_signed_char 1
#define CFI_type_short 2
#define CFI_type_int 3
#define CFI_type_long 4
#define CFI_type_long_long 5
#define CFI_type_size_t 6
#define CFI_type_int8_t 7
#define CFI_type_int16_t 8
#define CFI_type_int32_t 9
#define CFI_type_int64_t 10
#define CFI_type_int_least8_t 11
#define CFI_type_int_least16_t 12
#define CFI_type_int_least32_t 13
#define CFI_type_int_least64_t 14
#define CFI_type_int_fast8_t 15
#define CFI_type_int_fast16_t 16
#define CFI_type_int_fast32_t 17
#define CFI_type_int_fast64_t 18
#define CFI_type_intmax_t 19
#define CFI_type_intptr_t 20
#define CFI_type_ptrdiff_t 21
#define CFI_type_float 27
#define CFI_type_double 28
#define CFI_type_long_double 30
#define CFI_type_float_Complex 34
#define CFI_type_double_Complex 35
#define CFI_type_long_double_Complex 36
#define CFI_type_Bool 39
#define CFI_type_char 40
#define CFI_type_cptr 41
#define CFI_type_cfunptr 43
#define CFI_type_struct 42
#define CFI_type_other (-1)

/* Error codes */
#define CFI_SUCCESS 0
#define CFI_ERROR_BASE_ADDR_NULL 11
#define CFI_ERROR_BASE_ADDR_NOT_NULL 12
#define CFI_INVALID_ELEM_LEN 13
#define CFI_INVALID_RANK 14
#define CFI_INVALID_TYPE 15
#define CFI_INVALID_ATTRIBUTE 16
#define CFI_INVALID_EXTENT 17
#define CFI_INVALID_DESCRIPTOR 18
#define CFI_ERROR_MEM_ALLOCATION 19
#define CFI_ERROR_OUT_OF_BOUNDS 20

/*
 * Per-dimension descriptor.
 *
 * Standard order: {lower_bound, extent, sm}.
 * Matches ISO/IEC 18508 (Fortran 2018) Table 18.5 and Flang.
 */
typedef struct CFI_dim_t {
    CFI_index_t lower_bound;
    CFI_index_t extent;      /* -1 for assumed size */
    CFI_index_t sm;          /* stride multiplier in bytes */
} CFI_dim_t;

#ifdef __cplusplus
namespace cfi_internal {
/* C++ does not support flexible array members.  This template emulates one
 * by inheriting from CFI_dim_t and providing array-style access. */
extern "C++" template <typename T> struct FlexibleArray : T {
    T &operator[](int index) { return *(this + index); }
    const T &operator[](int index) const { return *(this + index); }
    operator T *() { return this; }
    operator const T *() const { return this; }
};
} /* namespace cfi_internal */
#endif

/*
 * Descriptor header members — factored into a macro so that both
 * CFI_cdesc_t and CFI_CDESC_T(rank) share the identical prefix.
 */
#define _CFI_CDESC_T_HEADER \
    void *base_addr; \
    int64_t elem_len; \
    int32_t version; \
    CFI_rank_t rank; \
    CFI_type_t type; \
    CFI_attribute_t attribute; \
    uint8_t extra;

/*
 * CFI array descriptor (C-interop layout).
 *
 * This is the standard CFI_cdesc_t without the internal `offset` field
 * that LFortran keeps in its LLVM IR struct.  At bind(C) boundaries the
 * codegen folds the offset into base_addr so C callers never see it.
 *
 * LLVM IR struct (internal):
 *   { ptr, i64, i32, i8, i8, i8, i8, i64, [rank x {i64,i64,i64}] }
 *                                         ^^^  offset kept internally
 *
 * C struct (this header):
 *   { ptr, i64, i32, u8, i8, u8, u8, [rank x {i64,i64,i64}] }
 */
typedef struct CFI_cdesc_t {
    _CFI_CDESC_T_HEADER
#ifdef __cplusplus
    cfi_internal::FlexibleArray<CFI_dim_t> dim;
#else
    CFI_dim_t dim[]; /* flexible array member (C99) */
#endif
} CFI_cdesc_t;

/* 18.5.4 — Storage macro for stack-allocated descriptors of a given rank.
 * Provides enough trailing storage for `_RANK` dimension descriptors. */
#ifdef __cplusplus
namespace cfi_internal {
extern "C++" template <int r> struct CdescStorage : public CFI_cdesc_t {
    static_assert(r > 1 && r <= CFI_MAX_RANK, "CFI_INVALID_RANK");
    CFI_dim_t dim[r - 1];
};
extern "C++" template <> struct CdescStorage<1> : public CFI_cdesc_t {};
extern "C++" template <> struct CdescStorage<0> : public CFI_cdesc_t {};
} /* namespace cfi_internal */
#define CFI_CDESC_T(_RANK) cfi_internal::CdescStorage<_RANK>
#else
#define CFI_CDESC_T(_RANK) \
    struct { \
        _CFI_CDESC_T_HEADER \
        CFI_dim_t dim[_RANK]; \
    }
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*
 * CFI_allocate — allocate memory for an allocatable or pointer descriptor.
 *
 * `lower` and `upper` are arrays of length `desc->rank` giving the
 * bounds for each dimension.  `elem_len` is used only for character
 * types (overrides desc->elem_len); pass 0 to keep the existing
 * elem_len.
 */
static inline int CFI_allocate(CFI_cdesc_t *desc,
        const CFI_index_t lower[], const CFI_index_t upper[],
        size_t elem_len) {
    if (!desc) return CFI_INVALID_DESCRIPTOR;
    if (desc->base_addr != NULL) return CFI_ERROR_BASE_ADDR_NOT_NULL;
    if (desc->attribute != CFI_attribute_allocatable &&
        desc->attribute != CFI_attribute_pointer)
        return CFI_INVALID_ATTRIBUTE;

    if (elem_len > 0) desc->elem_len = (int64_t)elem_len;

    size_t total = (size_t)desc->elem_len;
    for (int i = 0; i < desc->rank; i++) {
        desc->dim[i].lower_bound = lower[i];
        desc->dim[i].extent = upper[i] - lower[i] + 1;
        if (desc->dim[i].extent < 0) desc->dim[i].extent = 0;
    }
    for (int i = 0; i < desc->rank; i++) {
        /* Standard: sm is stride multiplier in bytes */
        desc->dim[i].sm = desc->elem_len;
        for (int j = 0; j < i; j++) {
            desc->dim[i].sm *= desc->dim[j].extent;
        }
        total *= (size_t)desc->dim[i].extent;
    }
    if (total == 0) total = 1;
    desc->base_addr = calloc(1, total);
    if (!desc->base_addr) return CFI_ERROR_MEM_ALLOCATION;
    return CFI_SUCCESS;
}

/*
 * CFI_deallocate — free memory previously allocated by CFI_allocate.
 */
static inline int CFI_deallocate(CFI_cdesc_t *desc) {
    if (!desc) return CFI_INVALID_DESCRIPTOR;
    if (desc->base_addr == NULL) return CFI_ERROR_BASE_ADDR_NULL;
    if (desc->attribute != CFI_attribute_allocatable &&
        desc->attribute != CFI_attribute_pointer)
        return CFI_INVALID_ATTRIBUTE;
    free(desc->base_addr);
    desc->base_addr = NULL;
    return CFI_SUCCESS;
}

/*
 * CFI_setpointer — make a pointer descriptor point at a target.
 *
 * If `source` is NULL the pointer is disassociated.
 * `lower_bounds`, if non-NULL, overrides the lower bounds (length
 * must equal source->rank).
 */
static inline int CFI_setpointer(CFI_cdesc_t *ptr,
        const CFI_cdesc_t *source,
        const CFI_index_t lower_bounds[]) {
    if (!ptr) return CFI_INVALID_DESCRIPTOR;
    if (ptr->attribute != CFI_attribute_pointer) return CFI_INVALID_ATTRIBUTE;

    if (source == NULL) {
        ptr->base_addr = NULL;
        return CFI_SUCCESS;
    }

    ptr->base_addr = source->base_addr;
    ptr->elem_len = source->elem_len;
    for (int i = 0; i < ptr->rank; i++) {
        ptr->dim[i].sm = source->dim[i].sm;
        ptr->dim[i].extent = source->dim[i].extent;
        if (lower_bounds) {
            ptr->dim[i].lower_bound = lower_bounds[i];
        } else {
            ptr->dim[i].lower_bound = source->dim[i].lower_bound;
        }
    }
    return CFI_SUCCESS;
}

/* Minimal establish function (inline, no runtime library needed) */
static inline int CFI_establish(CFI_cdesc_t *desc, void *base_addr,
        CFI_attribute_t attribute, CFI_type_t type,
        size_t elem_len, CFI_rank_t rank,
        const CFI_index_t extents[]) {
    if (!desc) return CFI_INVALID_DESCRIPTOR;
    if (rank > CFI_MAX_RANK) return CFI_INVALID_RANK;
    if (attribute != CFI_attribute_pointer &&
        attribute != CFI_attribute_allocatable &&
        attribute != CFI_attribute_other)
        return CFI_INVALID_ATTRIBUTE;

    /* Validate type code */
    switch (type) {
        case CFI_type_signed_char:
        case CFI_type_short:
        case CFI_type_int:
        case CFI_type_long:
        case CFI_type_long_long:
        case CFI_type_size_t:
        case CFI_type_int8_t:
        case CFI_type_int16_t:
        case CFI_type_int32_t:
        case CFI_type_int64_t:
        case CFI_type_int_least8_t:
        case CFI_type_int_least16_t:
        case CFI_type_int_least32_t:
        case CFI_type_int_least64_t:
        case CFI_type_int_fast8_t:
        case CFI_type_int_fast16_t:
        case CFI_type_int_fast32_t:
        case CFI_type_int_fast64_t:
        case CFI_type_intmax_t:
        case CFI_type_intptr_t:
        case CFI_type_ptrdiff_t:
        case CFI_type_float:
        case CFI_type_double:
        case CFI_type_long_double:
        case CFI_type_float_Complex:
        case CFI_type_double_Complex:
        case CFI_type_long_double_Complex:
        case CFI_type_Bool:
        case CFI_type_char:
        case CFI_type_cptr:
        case CFI_type_cfunptr:
        case CFI_type_struct:
        case CFI_type_other:
            break;
        default:
            return CFI_INVALID_TYPE;
    }

    /* Per F2018, elem_len is only used for character types;
       for other types, infer the element length from the type code. */
    if (elem_len == 0) {
        switch (type) {
            case CFI_type_signed_char: elem_len = sizeof(signed char); break;
            case CFI_type_short:       elem_len = sizeof(short); break;
            case CFI_type_int:         elem_len = sizeof(int); break;
            case CFI_type_long:        elem_len = sizeof(long); break;
            case CFI_type_long_long:   elem_len = sizeof(long long); break;
            case CFI_type_size_t:      elem_len = sizeof(size_t); break;
            case CFI_type_int8_t:      elem_len = 1; break;
            case CFI_type_int16_t:     elem_len = 2; break;
            case CFI_type_int32_t:     elem_len = 4; break;
            case CFI_type_int64_t:     elem_len = 8; break;
            case CFI_type_int_least8_t:  elem_len = sizeof(int_least8_t); break;
            case CFI_type_int_least16_t: elem_len = sizeof(int_least16_t); break;
            case CFI_type_int_least32_t: elem_len = sizeof(int_least32_t); break;
            case CFI_type_int_least64_t: elem_len = sizeof(int_least64_t); break;
            case CFI_type_int_fast8_t:   elem_len = sizeof(int_fast8_t); break;
            case CFI_type_int_fast16_t:  elem_len = sizeof(int_fast16_t); break;
            case CFI_type_int_fast32_t:  elem_len = sizeof(int_fast32_t); break;
            case CFI_type_int_fast64_t:  elem_len = sizeof(int_fast64_t); break;
            case CFI_type_intmax_t:      elem_len = sizeof(intmax_t); break;
            case CFI_type_intptr_t:      elem_len = sizeof(intptr_t); break;
            case CFI_type_ptrdiff_t:     elem_len = sizeof(ptrdiff_t); break;
            case CFI_type_float:       elem_len = sizeof(float); break;
            case CFI_type_double:      elem_len = sizeof(double); break;
            case CFI_type_long_double: elem_len = sizeof(long double); break;
            case CFI_type_float_Complex:  elem_len = 2 * sizeof(float); break;
            case CFI_type_double_Complex: elem_len = 2 * sizeof(double); break;
            case CFI_type_long_double_Complex: elem_len = 2 * sizeof(long double); break;
            case CFI_type_Bool:        elem_len = sizeof(uint8_t); break;
            case CFI_type_char:        elem_len = 1; break;
            case CFI_type_cptr:        elem_len = sizeof(void *); break;
            default: break;
        }
    }

    desc->base_addr = base_addr;
    desc->elem_len = (int64_t)elem_len;
    desc->version = CFI_VERSION;
    desc->rank = rank;
    desc->type = type;
    desc->attribute = attribute;
    for (CFI_rank_t i = 0; i < rank; i++) {
        desc->dim[i].lower_bound = 0;
        desc->dim[i].extent = extents ? extents[i] : 0;
        /* Standard: sm is stride multiplier in bytes */
        desc->dim[i].sm = (int64_t)elem_len;
        for (CFI_rank_t j = 0; j < i; j++) {
            desc->dim[i].sm *= desc->dim[j].extent;
        }
    }
    return CFI_SUCCESS;
}

/*
 * CFI_address — compute the address of an element given subscripts.
 *
 * `subscripts` is an array of length `desc->rank` containing 0-based indices
 * relative to each dimension's lower_bound.
 */
static inline void *CFI_address(const CFI_cdesc_t *desc,
        const CFI_index_t subscripts[]) {
    if (!desc || !desc->base_addr) return NULL;
    ptrdiff_t byte_offset = 0;
    for (int i = 0; i < desc->rank; i++) {
        CFI_index_t idx = subscripts[i] - desc->dim[i].lower_bound;
        byte_offset += idx * desc->dim[i].sm;
    }
    return (char *)desc->base_addr + byte_offset;
}

/*
 * CFI_section — create a section descriptor from a source descriptor.
 *
 * `lower`, `upper`, `strides` are arrays of length `source->rank`.
 * The result descriptor must be established first (with correct rank/type).
 */
static inline int CFI_section(CFI_cdesc_t *result,
        const CFI_cdesc_t *source,
        const CFI_index_t lower[],
        const CFI_index_t upper[],
        const CFI_index_t strides[]) {
    if (!result || !source) return CFI_INVALID_DESCRIPTOR;
    if (!source->base_addr) return CFI_ERROR_BASE_ADDR_NULL;

    int res_rank = result->rank;
    result->elem_len = source->elem_len;
    result->type = source->type;

    ptrdiff_t byte_offset = 0;
    int rd = 0;
    for (int i = 0; i < source->rank; i++) {
        CFI_index_t lb = lower ? lower[i] : source->dim[i].lower_bound;
        CFI_index_t ub = upper ? upper[i] : source->dim[i].lower_bound +
                                              source->dim[i].extent - 1;
        CFI_index_t st = strides ? strides[i] : 1;

        byte_offset += (lb - source->dim[i].lower_bound) *
                       source->dim[i].sm;

        if (rd < res_rank) {
            CFI_index_t ext;
            if (st > 0) {
                ext = (ub - lb + st) / st;
            } else {
                ext = (lb - ub - st) / (-st);
            }
            if (ext < 0) ext = 0;
            result->dim[rd].lower_bound = 0;
            result->dim[rd].extent = ext;
            result->dim[rd].sm = source->dim[i].sm * st;
            rd++;
        }
    }
    result->base_addr = (char *)source->base_addr + byte_offset;
    return CFI_SUCCESS;
}

/*
 * CFI_is_contiguous — check whether a descriptor describes contiguous storage.
 *
 * Returns 1 if contiguous, 0 otherwise (or if desc is NULL).
 */
static inline int CFI_is_contiguous(const CFI_cdesc_t *desc) {
    if (!desc) return 0;
    if (desc->rank == 0) return 1;
    if (!desc->base_addr) return 1;

    CFI_index_t expected_sm = desc->elem_len;
    for (int i = 0; i < desc->rank; i++) {
        if (desc->dim[i].sm != expected_sm) return 0;
        expected_sm *= desc->dim[i].extent;
    }
    return 1;
}

/*
 * CFI_select_part — select a component of each element in the source array.
 *
 * `displacement` is the byte offset of the component within each element.
 * `elem_len` is the size in bytes of the selected component.
 * The result descriptor must be established first with the correct rank and type.
 */
static inline int CFI_select_part(CFI_cdesc_t *result,
        const CFI_cdesc_t *source,
        size_t displacement, size_t elem_len) {
    if (!result || !source) return CFI_INVALID_DESCRIPTOR;
    if (!source->base_addr) return CFI_ERROR_BASE_ADDR_NULL;

    result->base_addr = (char *)source->base_addr + displacement;
    result->elem_len = (int64_t)elem_len;

    for (int i = 0; i < result->rank; i++) {
        result->dim[i].lower_bound = 0;
        result->dim[i].extent = source->dim[i].extent;
        result->dim[i].sm = source->dim[i].sm;
    }

    return CFI_SUCCESS;
}

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* CFI_ISO_FORTRAN_BINDING_H_ */
