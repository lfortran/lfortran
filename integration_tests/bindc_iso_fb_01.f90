! Consolidated integration test: bindc_iso_fb_01
!
! Merged from:
!   - bindc_18.f90: Basic array descriptors (int32/int64/float/double sums,
!                   assumed-rank, allocatable, pointer, sections, optional)
!   - bindc_19.f90: Additional type coverage (complex float/double, logical,
!                   CONTIGUOUS attribute)
!   - bindc_24.f90: Scalar types and VALUE attribute (int8, int16, short, long,
!                   long_long, size_t, intptr_t, ptrdiff_t, bool, complex,
!                   character, small-int arrays, complex return values)
!
! Features covered:
!   - CFI descriptors for assumed-shape arrays (ranks 1-3)
!   - Assumed-rank (type(*), dimension(..))
!   - Allocatable and pointer arrays through C
!   - Array sections and contiguity
!   - Optional arguments
!   - Complex, logical, and character types through descriptors
!   - CONTIGUOUS attribute on assumed-shape
!   - Scalar VALUE attribute for all ISO C binding integer kinds
!   - VALUE attribute with logical, complex, character scalars
!   - Functions returning complex values

module bindc_18_mod
    use iso_c_binding, only: c_int, c_int32_t, c_int64_t, c_float, c_double
    implicit none

    interface
        ! ---- int32 sum: ranks 1, 2, 3 and assumed rank ----
        integer(c_int32_t) function c_sum_int32_1d(a) bind(C, name="c_sum_int32_1d")
            import :: c_int32_t
            integer(c_int32_t), intent(in) :: a(:)
        end function
        integer(c_int32_t) function c_sum_int32_2d(a) bind(C, name="c_sum_int32_2d")
            import :: c_int32_t
            integer(c_int32_t), intent(in) :: a(:,:)
        end function
        integer(c_int32_t) function c_sum_int32_3d(a) bind(C, name="c_sum_int32_3d")
            import :: c_int32_t
            integer(c_int32_t), intent(in) :: a(:,:,:)
        end function
        integer(c_int32_t) function c_sum_int32_ar(a) bind(C, name="c_sum_int32_ar")
            import :: c_int32_t
            integer(c_int32_t), intent(in) :: a(..)
        end function

        ! ---- int64 sum: ranks 1, 2, 3 ----
        integer(c_int64_t) function c_sum_int64_1d(a) bind(C, name="c_sum_int64_1d")
            import :: c_int64_t
            integer(c_int64_t), intent(in) :: a(:)
        end function
        integer(c_int64_t) function c_sum_int64_2d(a) bind(C, name="c_sum_int64_2d")
            import :: c_int64_t
            integer(c_int64_t), intent(in) :: a(:,:)
        end function
        integer(c_int64_t) function c_sum_int64_3d(a) bind(C, name="c_sum_int64_3d")
            import :: c_int64_t
            integer(c_int64_t), intent(in) :: a(:,:,:)
        end function

        ! ---- float sum: ranks 1, 2, 3 ----
        real(c_float) function c_sum_float_1d(a) bind(C, name="c_sum_float_1d")
            import :: c_float
            real(c_float), intent(in) :: a(:)
        end function
        real(c_float) function c_sum_float_2d(a) bind(C, name="c_sum_float_2d")
            import :: c_float
            real(c_float), intent(in) :: a(:,:)
        end function
        real(c_float) function c_sum_float_3d(a) bind(C, name="c_sum_float_3d")
            import :: c_float
            real(c_float), intent(in) :: a(:,:,:)
        end function

        ! ---- double sum: ranks 1, 2, 3 ----
        real(c_double) function c_sum_double_1d(a) bind(C, name="c_sum_double_1d")
            import :: c_double
            real(c_double), intent(in) :: a(:)
        end function
        real(c_double) function c_sum_double_2d(a) bind(C, name="c_sum_double_2d")
            import :: c_double
            real(c_double), intent(in) :: a(:,:)
        end function
        real(c_double) function c_sum_double_3d(a) bind(C, name="c_sum_double_3d")
            import :: c_double
            real(c_double), intent(in) :: a(:,:,:)
        end function

        ! ---- int32 double-in-place: ranks 1, 2, 3 ----
        subroutine c_scale_int32_1d(a) bind(C, name="c_double_int32_1d")
            import :: c_int32_t
            integer(c_int32_t), intent(inout) :: a(:)
        end subroutine
        subroutine c_scale_int32_2d(a) bind(C, name="c_double_int32_2d")
            import :: c_int32_t
            integer(c_int32_t), intent(inout) :: a(:,:)
        end subroutine
        subroutine c_scale_int32_3d(a) bind(C, name="c_double_int32_3d")
            import :: c_int32_t
            integer(c_int32_t), intent(inout) :: a(:,:,:)
        end subroutine

        ! ---- assumed-rank queries: type(*), dimension(..) ----
        integer(c_int) function c_get_rank(a) bind(C, name="c_get_rank")
            import :: c_int
            type(*), intent(in) :: a(..)
        end function

        integer(c_int) function c_get_elem_size(a) bind(C, name="c_get_elem_size")
            import :: c_int
            type(*), intent(in) :: a(..)
        end function

        ! ---- allocatable arrays ----
        integer(c_int32_t) function c_sum_alloc_1d(a) bind(C, name="c_sum_alloc_1d")
            import :: c_int32_t
            integer(c_int32_t), allocatable, intent(in) :: a(:)
        end function
        integer(c_int) function c_attr_alloc(a) bind(C, name="c_attr_alloc")
            import :: c_int, c_int32_t
            integer(c_int32_t), allocatable, intent(in) :: a(:)
        end function
        subroutine c_double_alloc_1d(a) bind(C, name="c_double_alloc_1d")
            import :: c_int32_t
            integer(c_int32_t), allocatable, intent(inout) :: a(:)
        end subroutine

        ! ---- pointer arrays ----
        integer(c_int32_t) function c_sum_ptr_1d(a) bind(C, name="c_sum_ptr_1d")
            import :: c_int32_t
            integer(c_int32_t), pointer, intent(in) :: a(:)
        end function
        integer(c_int) function c_attr_ptr(a) bind(C, name="c_attr_ptr")
            import :: c_int, c_int32_t
            integer(c_int32_t), pointer, intent(in) :: a(:)
        end function

        ! ---- attribute / contiguity for assumed-shape (other) ----
        integer(c_int) function c_attr_other(a) bind(C, name="c_attr_other")
            import :: c_int, c_int32_t
            integer(c_int32_t), intent(in) :: a(:)
        end function
        integer(c_int) function c_is_contiguous(a) bind(C, name="c_is_contiguous")
            import :: c_int, c_int32_t
            integer(c_int32_t), intent(in) :: a(:)
        end function

        ! ---- optional argument ----
        integer(c_int) function c_is_present(a) bind(C, name="c_is_present")
            import :: c_int, c_int32_t
            integer(c_int32_t), optional, intent(in) :: a(:)
        end function
    end interface
end module

module bindc_19_mod
    use iso_c_binding, only: c_int, c_float, c_double, c_bool, &
                             c_float_complex, c_double_complex
    implicit none

    interface
        ! ---- complex(c_float_complex) arrays ----
        subroutine c19_sum_cfloat_1d(a, re, im) bind(C, name="c19_sum_cfloat_1d")
            import :: c_float_complex, c_float
            complex(c_float_complex), intent(in) :: a(:)
            real(c_float), intent(out) :: re, im
        end subroutine

        subroutine c19_sum_cfloat_2d(a, re, im) bind(C, name="c19_sum_cfloat_2d")
            import :: c_float_complex, c_float
            complex(c_float_complex), intent(in) :: a(:,:)
            real(c_float), intent(out) :: re, im
        end subroutine

        subroutine c19_scale_cfloat_1d(a) bind(C, name="c19_scale_cfloat_1d")
            import :: c_float_complex
            complex(c_float_complex), intent(inout) :: a(:)
        end subroutine

        ! ---- complex(c_double_complex) arrays ----
        subroutine c19_sum_cdouble_1d(a, re, im) bind(C, name="c19_sum_cdouble_1d")
            import :: c_double_complex, c_double
            complex(c_double_complex), intent(in) :: a(:)
            real(c_double), intent(out) :: re, im
        end subroutine

        subroutine c19_sum_cdouble_2d(a, re, im) bind(C, name="c19_sum_cdouble_2d")
            import :: c_double_complex, c_double
            complex(c_double_complex), intent(in) :: a(:,:)
            real(c_double), intent(out) :: re, im
        end subroutine

        ! ---- logical(c_bool) arrays ----
        integer(c_int) function c19_count_true_1d(a) bind(C, name="c19_count_true_1d")
            import :: c_int, c_bool
            logical(c_bool), intent(in) :: a(:)
        end function

        integer(c_int) function c19_count_true_2d(a) bind(C, name="c19_count_true_2d")
            import :: c_int, c_bool
            logical(c_bool), intent(in) :: a(:,:)
        end function

        subroutine c19_flip_bool_1d(a) bind(C, name="c19_flip_bool_1d")
            import :: c_bool
            logical(c_bool), intent(inout) :: a(:)
        end subroutine

        ! ---- CONTIGUOUS attribute on assumed-shape ----
        integer(c_int) function c19_sum_contiguous(a) bind(C, name="c19_sum_contiguous")
            import :: c_int
            integer(c_int), contiguous, intent(in) :: a(:)
        end function

        integer(c_int) function c19_is_contiguous_check(a) &
                bind(C, name="c19_is_contiguous_check")
            import :: c_int
            integer(c_int), contiguous, intent(in) :: a(:)
        end function
    end interface
end module

module bindc_24_mod
    use iso_c_binding, only: c_int8_t, c_int16_t, c_short, c_long, &
        c_long_long, c_size_t, c_intptr_t, c_ptrdiff_t, &
        c_bool, c_float_complex, c_double_complex, c_char, &
        c_int, c_float, c_double, c_int32_t
    implicit none

    interface
        ! ---- int8 ----
        integer(c_int8_t) function c24_add_int8(a, b) &
                bind(C, name="c24_add_int8")
            import :: c_int8_t
            integer(c_int8_t), value :: a, b
        end function

        subroutine c24_double_int8(a) bind(C, name="c24_double_int8")
            import :: c_int8_t
            integer(c_int8_t), intent(inout) :: a
        end subroutine

        ! ---- int16 ----
        integer(c_int16_t) function c24_add_int16(a, b) &
                bind(C, name="c24_add_int16")
            import :: c_int16_t
            integer(c_int16_t), value :: a, b
        end function

        subroutine c24_double_int16(a) bind(C, name="c24_double_int16")
            import :: c_int16_t
            integer(c_int16_t), intent(inout) :: a
        end subroutine

        ! ---- short ----
        integer(c_short) function c24_add_short(a, b) &
                bind(C, name="c24_add_short")
            import :: c_short
            integer(c_short), value :: a, b
        end function

        ! ---- long ----
        integer(c_long) function c24_add_long(a, b) &
                bind(C, name="c24_add_long")
            import :: c_long
            integer(c_long), value :: a, b
        end function

        ! ---- long_long ----
        integer(c_long_long) function c24_add_long_long(a, b) &
                bind(C, name="c24_add_long_long")
            import :: c_long_long
            integer(c_long_long), value :: a, b
        end function

        ! ---- size_t ----
        integer(c_size_t) function c24_add_size_t(a, b) &
                bind(C, name="c24_add_size_t")
            import :: c_size_t
            integer(c_size_t), value :: a, b
        end function

        ! ---- intptr_t ----
        integer(c_intptr_t) function c24_add_intptr(a, b) &
                bind(C, name="c24_add_intptr")
            import :: c_intptr_t
            integer(c_intptr_t), value :: a, b
        end function

        ! ---- ptrdiff_t ----
        integer(c_ptrdiff_t) function c24_add_ptrdiff(a, b) &
                bind(C, name="c24_add_ptrdiff")
            import :: c_ptrdiff_t
            integer(c_ptrdiff_t), value :: a, b
        end function

        ! ---- VALUE with logical(c_bool) ----
        integer(c_int) function c24_bool_to_int(x) &
                bind(C, name="c24_bool_to_int")
            import :: c_int, c_bool
            logical(c_bool), value :: x
        end function

        ! ---- VALUE with complex(c_float_complex) ----
        real(c_float) function c24_cabs_float(z) &
                bind(C, name="c24_cabs_float")
            import :: c_float, c_float_complex
            complex(c_float_complex), value :: z
        end function

        ! ---- VALUE with complex(c_double_complex) ----
        real(c_double) function c24_cabs_double(z) &
                bind(C, name="c24_cabs_double")
            import :: c_double, c_double_complex
            complex(c_double_complex), value :: z
        end function

        ! ---- VALUE with character(c_char) ----
        integer(c_int) function c24_char_to_int(ch) &
                bind(C, name="c24_char_to_int")
            import :: c_int, c_char
            character(kind=c_char, len=1), value :: ch
        end function

        ! ---- int8/int16 arrays through descriptors ----
        integer(c_int) function c24_sum_int8_1d(a) &
                bind(C, name="c24_sum_int8_1d")
            import :: c_int, c_int8_t
            integer(c_int8_t), intent(in) :: a(:)
        end function

        integer(c_int) function c24_sum_int16_1d(a) &
                bind(C, name="c24_sum_int16_1d")
            import :: c_int, c_int16_t
            integer(c_int16_t), intent(in) :: a(:)
        end function

        ! ---- function returning complex ----
        complex(c_float_complex) function c24_make_complex(re, im) &
                bind(C, name="c24_make_complex")
            import :: c_float_complex, c_float
            real(c_float), value :: re, im
        end function

        complex(c_double_complex) function c24_make_dcomplex(re, im) &
                bind(C, name="c24_make_dcomplex")
            import :: c_double_complex, c_double
            real(c_double), value :: re, im
        end function
    end interface
end module

program bindc_iso_fb_01
    use bindc_18_mod
    use bindc_19_mod
    use bindc_24_mod
    use iso_c_binding
    implicit none

    ! --- Tests from bindc_18 ---
    call test_sum_int32()
    call test_sum_int64()
    call test_sum_float()
    call test_sum_double()
    call test_double_inplace()
    call test_assumed_rank()
    call test_allocatable()
    call test_pointer()
    call test_sections()
    call test_optional()

    ! --- Tests from bindc_19 ---
    call test_complex_float()
    call test_complex_double()
    call test_logical()
    call test_contiguous()

    ! --- Tests from bindc_24 ---
    call test_int8()
    call test_int16()
    call test_short()
    call test_long()
    call test_long_long()
    call test_size_t()
    call test_intptr()
    call test_ptrdiff()
    call test_bool_value()
    call test_complex_value()
    call test_char_value()
    call test_small_int_arrays()
    call test_complex_return()

    print *, "All bindc_iso_fb_01 tests passed."

contains

    ! ==================================================================
    ! Tests from bindc_18: basic array descriptors
    ! ==================================================================

    subroutine test_sum_int32()
        integer(c_int32_t) :: a1(4), a2(2,3), a3(2,3,2)
        integer :: i

        a1 = [1, 2, 3, 4]
        a2 = reshape([1, 2, 3, 4, 5, 6], [2, 3])
        a3 = reshape([(i, i=1,12)], [2, 3, 2])

        if (c_sum_int32_1d(a1) /= 10) error stop "FAIL: int32 1d sum"
        if (c_sum_int32_2d(a2) /= 21) error stop "FAIL: int32 2d sum"
        if (c_sum_int32_3d(a3) /= 78) error stop "FAIL: int32 3d sum"
    end subroutine

    subroutine test_sum_int64()
        integer(c_int64_t) :: a1(4), a2(2,3), a3(2,3,2)
        integer :: i

        a1 = [1_c_int64_t, 2_c_int64_t, 3_c_int64_t, 4_c_int64_t]
        a2 = reshape([1_c_int64_t, 2_c_int64_t, 3_c_int64_t, &
                      4_c_int64_t, 5_c_int64_t, 6_c_int64_t], [2, 3])
        a3 = reshape([(int(i, c_int64_t), i=1,12)], [2, 3, 2])

        if (c_sum_int64_1d(a1) /= 10_c_int64_t) error stop "FAIL: int64 1d sum"
        if (c_sum_int64_2d(a2) /= 21_c_int64_t) error stop "FAIL: int64 2d sum"
        if (c_sum_int64_3d(a3) /= 78_c_int64_t) error stop "FAIL: int64 3d sum"
    end subroutine

    subroutine test_sum_float()
        real(c_float) :: a1(4), a2(2,3), a3(2,3,2)
        integer :: i

        a1 = [1.0, 2.0, 3.0, 4.0]
        a2 = reshape([1.0, 2.0, 3.0, 4.0, 5.0, 6.0], [2, 3])
        a3 = reshape([(real(i), i=1,12)], [2, 3, 2])

        if (abs(c_sum_float_1d(a1) - 10.0) > 1.0e-5) error stop "FAIL: float 1d sum"
        if (abs(c_sum_float_2d(a2) - 21.0) > 1.0e-5) error stop "FAIL: float 2d sum"
        if (abs(c_sum_float_3d(a3) - 78.0) > 1.0e-5) error stop "FAIL: float 3d sum"
    end subroutine

    subroutine test_sum_double()
        real(c_double) :: a1(4), a2(2,3), a3(2,3,2)
        integer :: i

        a1 = [1.0d0, 2.0d0, 3.0d0, 4.0d0]
        a2 = reshape([1.0d0, 2.0d0, 3.0d0, 4.0d0, 5.0d0, 6.0d0], [2, 3])
        a3 = reshape([(dble(i), i=1,12)], [2, 3, 2])

        if (abs(c_sum_double_1d(a1) - 10.0d0) > 1.0d-10) error stop "FAIL: double 1d sum"
        if (abs(c_sum_double_2d(a2) - 21.0d0) > 1.0d-10) error stop "FAIL: double 2d sum"
        if (abs(c_sum_double_3d(a3) - 78.0d0) > 1.0d-10) error stop "FAIL: double 3d sum"
    end subroutine

    subroutine test_double_inplace()
        integer(c_int32_t) :: a1(4), a2(2,3), a3(2,3,2)
        integer :: i

        ! rank 1
        a1 = [1, 2, 3, 4]
        call c_scale_int32_1d(a1)
        if (a1(1) /= 2 .or. a1(4) /= 8) error stop "FAIL: scale int32 1d"
        if (c_sum_int32_1d(a1) /= 20) error stop "FAIL: scale int32 1d sum"

        ! rank 2
        a2 = reshape([1, 2, 3, 4, 5, 6], [2, 3])
        call c_scale_int32_2d(a2)
        if (a2(1,1) /= 2 .or. a2(2,3) /= 12) error stop "FAIL: scale int32 2d"
        if (c_sum_int32_2d(a2) /= 42) error stop "FAIL: scale int32 2d sum"

        ! rank 3
        a3 = reshape([(i, i=1,12)], [2, 3, 2])
        call c_scale_int32_3d(a3)
        if (a3(1,1,1) /= 2 .or. a3(2,3,2) /= 24) error stop "FAIL: scale int32 3d"
        if (c_sum_int32_3d(a3) /= 156) error stop "FAIL: scale int32 3d sum"
    end subroutine

    subroutine test_assumed_rank()
        integer(c_int32_t) :: ai1(4), ai2(2,3), ai3(2,3,2)
        real(c_double)     :: ad1(3)
        integer(c_int64_t) :: al1(2)
        integer :: i

        ai1 = [1, 2, 3, 4]
        ai2 = reshape([1, 2, 3, 4, 5, 6], [2, 3])
        ai3 = reshape([(i, i=1,12)], [2, 3, 2])
        ad1 = [1.0d0, 2.0d0, 3.0d0]
        al1 = [10_c_int64_t, 20_c_int64_t]

        ! rank query
        if (c_get_rank(ai1) /= 1) error stop "FAIL: rank of ai1"
        if (c_get_rank(ai2) /= 2) error stop "FAIL: rank of ai2"
        if (c_get_rank(ai3) /= 3) error stop "FAIL: rank of ai3"
        if (c_get_rank(ad1) /= 1) error stop "FAIL: rank of ad1"
        if (c_get_rank(al1) /= 1) error stop "FAIL: rank of al1"

        ! element size query
        if (c_get_elem_size(ai1) /= 4) error stop "FAIL: elem_size int32"
        if (c_get_elem_size(ad1) /= 8) error stop "FAIL: elem_size double"
        if (c_get_elem_size(al1) /= 8) error stop "FAIL: elem_size int64"

        ! assumed-rank sum (int32 passed as rank 1, 2, 3)
        if (c_sum_int32_ar(ai1) /= 10) error stop "FAIL: assumed-rank int32 1d"
        if (c_sum_int32_ar(ai2) /= 21) error stop "FAIL: assumed-rank int32 2d"
        if (c_sum_int32_ar(ai3) /= 78) error stop "FAIL: assumed-rank int32 3d"
    end subroutine

    subroutine test_allocatable()
        integer(c_int32_t), allocatable :: a(:)

        allocate(a(4))
        a = [1, 2, 3, 4]

        ! sum through C
        if (c_sum_alloc_1d(a) /= 10) error stop "FAIL: alloc sum"

        ! attribute == CFI_attribute_allocatable
        if (c_attr_alloc(a) /= 1) error stop "FAIL: alloc attribute"

        ! in-place modification through C
        call c_double_alloc_1d(a)
        if (a(1) /= 2 .or. a(4) /= 8) error stop "FAIL: alloc double"
        if (c_sum_alloc_1d(a) /= 20) error stop "FAIL: alloc double sum"

        deallocate(a)
    end subroutine

    subroutine test_pointer()
        integer(c_int32_t), target :: tgt(6)
        integer(c_int32_t), pointer :: p(:)

        tgt = [10, 20, 30, 40, 50, 60]
        p => tgt

        ! sum full pointer
        if (c_sum_ptr_1d(p) /= 210) error stop "FAIL: ptr sum"

        ! attribute == CFI_attribute_pointer
        if (c_attr_ptr(p) /= 1) error stop "FAIL: ptr attribute"

        ! pointer to contiguous section
        p => tgt(2:4)
        if (c_sum_ptr_1d(p) /= 90) error stop "FAIL: ptr section sum"

        ! pointer to strided section
        p => tgt(1::2)
        if (c_sum_ptr_1d(p) /= 90) error stop "FAIL: ptr stride sum"
    end subroutine

    subroutine test_sections()
        integer(c_int32_t) :: arr(6)
        arr = [1, 2, 3, 4, 5, 6]

        ! stride-2: elements 1, 3, 5
        if (c_sum_int32_1d(arr(::2)) /= 9) error stop "FAIL: stride-2"

        ! contiguous subarray: elements 2, 3, 4
        if (c_sum_int32_1d(arr(2:4)) /= 9) error stop "FAIL: subarray"

        ! stride-3: elements 1, 4
        if (c_sum_int32_1d(arr(1::3)) /= 5) error stop "FAIL: stride-3"

        ! contiguity: full array is contiguous
        if (c_is_contiguous(arr) /= 1) error stop "FAIL: contiguous"

        ! contiguity: strided section is not contiguous
        if (c_is_contiguous(arr(::2)) /= 0) error stop "FAIL: non-contiguous"

        ! attribute: regular assumed-shape has CFI_attribute_other
        if (c_attr_other(arr) /= 1) error stop "FAIL: other attr"
    end subroutine

    subroutine test_optional()
        integer(c_int32_t) :: arr(3)
        arr = [1, 2, 3]

        ! present argument
        if (c_is_present(arr) /= 1) error stop "FAIL: present"

        ! absent optional argument
        if (c_is_present() /= 0) error stop "FAIL: absent"
    end subroutine

    ! ==================================================================
    ! Tests from bindc_19: complex, logical, CONTIGUOUS
    ! ==================================================================

    subroutine test_complex_float()
        complex(c_float_complex) :: a1(3), a2(2,2)
        real(c_float) :: re, im

        a1 = [(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)]

        call c19_sum_cfloat_1d(a1, re, im)
        if (abs(re - 9.0) > 1.0e-5) error stop "FAIL: cfloat 1d real"
        if (abs(im - 12.0) > 1.0e-5) error stop "FAIL: cfloat 1d imag"

        a2 = reshape([(1.0, 1.0), (2.0, 2.0), (3.0, 3.0), (4.0, 4.0)], [2, 2])
        call c19_sum_cfloat_2d(a2, re, im)
        if (abs(re - 10.0) > 1.0e-5) error stop "FAIL: cfloat 2d real"
        if (abs(im - 10.0) > 1.0e-5) error stop "FAIL: cfloat 2d imag"

        a1 = [(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)]
        call c19_scale_cfloat_1d(a1)
        if (abs(real(a1(1)) - 2.0) > 1.0e-5) error stop "FAIL: cfloat scale re"
        if (abs(aimag(a1(1)) - 4.0) > 1.0e-5) error stop "FAIL: cfloat scale im"
    end subroutine

    subroutine test_complex_double()
        complex(c_double_complex) :: a1(3), a2(2,2)
        real(c_double) :: re, im

        a1 = [(1.0d0, 2.0d0), (3.0d0, 4.0d0), (5.0d0, 6.0d0)]

        call c19_sum_cdouble_1d(a1, re, im)
        if (abs(re - 9.0d0) > 1.0d-10) error stop "FAIL: cdouble 1d real"
        if (abs(im - 12.0d0) > 1.0d-10) error stop "FAIL: cdouble 1d imag"

        a2 = reshape([(1.0d0, 1.0d0), (2.0d0, 2.0d0), &
                      (3.0d0, 3.0d0), (4.0d0, 4.0d0)], [2, 2])
        call c19_sum_cdouble_2d(a2, re, im)
        if (abs(re - 10.0d0) > 1.0d-10) error stop "FAIL: cdouble 2d real"
        if (abs(im - 10.0d0) > 1.0d-10) error stop "FAIL: cdouble 2d imag"
    end subroutine

    subroutine test_logical()
        logical(c_bool) :: a1(5), a2(2,3)

        a1 = [.true., .false., .true., .true., .false.]
        if (c19_count_true_1d(a1) /= 3) error stop "FAIL: bool count 1d"

        a2 = reshape([.true., .false., .true., .false., .true., .true.], [2, 3])
        if (c19_count_true_2d(a2) /= 4) error stop "FAIL: bool count 2d"

        a1 = [.true., .false., .true., .true., .false.]
        call c19_flip_bool_1d(a1)
        if (a1(1) .neqv. .false.) error stop "FAIL: flip 1"
        if (a1(2) .neqv. .true.) error stop "FAIL: flip 2"
    end subroutine

    subroutine test_contiguous()
        integer(c_int) :: arr(6)
        integer :: i

        arr = [(i, i=1,6)]

        if (c19_sum_contiguous(arr) /= 21) error stop "FAIL: contiguous sum"
        if (c19_is_contiguous_check(arr) /= 1) error stop "FAIL: contiguous check"
    end subroutine

    ! ==================================================================
    ! Tests from bindc_24: scalar types and VALUE attribute
    ! ==================================================================

    subroutine test_int8()
        integer(c_int8_t) :: a, b, r, x
        a = 10_c_int8_t
        b = 20_c_int8_t
        r = c24_add_int8(a, b)
        if (r /= 30_c_int8_t) error stop "FAIL: int8 add"

        x = 7_c_int8_t
        call c24_double_int8(x)
        if (x /= 14_c_int8_t) error stop "FAIL: int8 double"
    end subroutine

    subroutine test_int16()
        integer(c_int16_t) :: a, b, r, x
        a = 1000_c_int16_t
        b = 2000_c_int16_t
        r = c24_add_int16(a, b)
        if (r /= 3000_c_int16_t) error stop "FAIL: int16 add"

        x = 123_c_int16_t
        call c24_double_int16(x)
        if (x /= 246_c_int16_t) error stop "FAIL: int16 double"
    end subroutine

    subroutine test_short()
        integer(c_short) :: r
        r = c24_add_short(100_c_short, 200_c_short)
        if (r /= 300_c_short) error stop "FAIL: short add"
    end subroutine

    subroutine test_long()
        integer(c_long) :: r
        r = c24_add_long(100000_c_long, 200000_c_long)
        if (r /= 300000_c_long) error stop "FAIL: long add"
    end subroutine

    subroutine test_long_long()
        integer(c_long_long) :: r
        r = c24_add_long_long(1000000000_c_long_long, &
                              2000000000_c_long_long)
        if (r /= 3000000000_c_long_long) error stop "FAIL: long_long add"
    end subroutine

    subroutine test_size_t()
        integer(c_size_t) :: r
        r = c24_add_size_t(100_c_size_t, 200_c_size_t)
        if (r /= 300_c_size_t) error stop "FAIL: size_t add"
    end subroutine

    subroutine test_intptr()
        integer(c_intptr_t) :: r
        r = c24_add_intptr(1000_c_intptr_t, 2000_c_intptr_t)
        if (r /= 3000_c_intptr_t) error stop "FAIL: intptr add"
    end subroutine

    subroutine test_ptrdiff()
        integer(c_ptrdiff_t) :: r
        r = c24_add_ptrdiff(500_c_ptrdiff_t, 600_c_ptrdiff_t)
        if (r /= 1100_c_ptrdiff_t) error stop "FAIL: ptrdiff add"
    end subroutine

    subroutine test_bool_value()
        logical(c_bool) :: t, f
        t = .true.
        f = .false.
        if (c24_bool_to_int(t) /= 1) error stop "FAIL: bool true"
        if (c24_bool_to_int(f) /= 0) error stop "FAIL: bool false"
    end subroutine

    subroutine test_complex_value()
        complex(c_float_complex) :: zf
        complex(c_double_complex) :: zd
        real(c_float) :: rf
        real(c_double) :: rd

        zf = (3.0, 4.0)
        rf = c24_cabs_float(zf)
        if (abs(rf - 5.0) > 1.0e-5) error stop "FAIL: cabs float"

        zd = (3.0d0, 4.0d0)
        rd = c24_cabs_double(zd)
        if (abs(rd - 5.0d0) > 1.0d-10) error stop "FAIL: cabs double"
    end subroutine

    subroutine test_char_value()
        if (c24_char_to_int('A') /= 65) error stop "FAIL: char A"
        if (c24_char_to_int('0') /= 48) error stop "FAIL: char 0"
    end subroutine

    subroutine test_small_int_arrays()
        integer(c_int8_t)  :: a8(4)
        integer(c_int16_t) :: a16(4)

        a8 = [1_c_int8_t, 2_c_int8_t, 3_c_int8_t, 4_c_int8_t]
        if (c24_sum_int8_1d(a8) /= 10) error stop "FAIL: int8 array sum"

        a16 = [100_c_int16_t, 200_c_int16_t, 300_c_int16_t, 400_c_int16_t]
        if (c24_sum_int16_1d(a16) /= 1000) error stop "FAIL: int16 array sum"
    end subroutine

    subroutine test_complex_return()
        complex(c_float_complex) :: zf
        complex(c_double_complex) :: zd

        zf = c24_make_complex(3.0, 4.0)
        if (abs(real(zf) - 3.0) > 1.0e-5) error stop "FAIL: make_complex re"
        if (abs(aimag(zf) - 4.0) > 1.0e-5) error stop "FAIL: make_complex im"

        zd = c24_make_dcomplex(5.0d0, 6.0d0)
        if (abs(real(zd) - 5.0d0) > 1.0d-10) error stop "FAIL: make_dcomplex re"
        if (abs(aimag(zd) - 6.0d0) > 1.0d-10) error stop "FAIL: make_dcomplex im"
    end subroutine

end program
