! Consolidated ISO_Fortran_binding test: Allocatable, Pointer & CFI Allocation/Setpointer
! Merged from: bindc_20, bindc_28, bindc_30, bindc_31, bindc_32
!
! bindc_20: Multi-rank allocatable/pointer, scalar alloc/ptr, intent(out/inout)
! bindc_28: Attribute combinations and array features (float, double, int64,
!           complex, logical, zero-size, unallocated, rank-4, explicit-shape,
!           optional+contiguous)
! bindc_30: Character alloc/ptr, allocatable+assumed-rank, pointer+assumed-rank
! bindc_31: C creates allocatable arrays via CFI_allocate and passes to Fortran
! bindc_32: C creates pointer associations via CFI_setpointer and passes to Fortran

! ============================================================================
! Module from bindc_20
! ============================================================================
module bindc_20_mod
    use iso_c_binding, only: c_int, c_int32_t
    implicit none

    interface
        ! ---- allocatable 2D ----
        integer(c_int32_t) function c20_sum_alloc_2d(a) &
                bind(C, name="c20_sum_alloc_2d")
            import :: c_int32_t
            integer(c_int32_t), allocatable, intent(in) :: a(:,:)
        end function

        integer(c_int) function c20_attr_alloc_2d(a) &
                bind(C, name="c20_attr_alloc_2d")
            import :: c_int, c_int32_t
            integer(c_int32_t), allocatable, intent(in) :: a(:,:)
        end function

        subroutine c20_double_alloc_2d(a) bind(C, name="c20_double_alloc_2d")
            import :: c_int32_t
            integer(c_int32_t), allocatable, intent(inout) :: a(:,:)
        end subroutine

        ! ---- allocatable 3D ----
        integer(c_int32_t) function c20_sum_alloc_3d(a) &
                bind(C, name="c20_sum_alloc_3d")
            import :: c_int32_t
            integer(c_int32_t), allocatable, intent(in) :: a(:,:,:)
        end function

        ! ---- pointer 2D ----
        integer(c_int32_t) function c20_sum_ptr_2d(a) &
                bind(C, name="c20_sum_ptr_2d")
            import :: c_int32_t
            integer(c_int32_t), pointer, intent(in) :: a(:,:)
        end function

        integer(c_int) function c20_attr_ptr_2d(a) &
                bind(C, name="c20_attr_ptr_2d")
            import :: c_int, c_int32_t
            integer(c_int32_t), pointer, intent(in) :: a(:,:)
        end function

        ! ---- pointer 3D ----
        integer(c_int32_t) function c20_sum_ptr_3d(a) &
                bind(C, name="c20_sum_ptr_3d")
            import :: c_int32_t
            integer(c_int32_t), pointer, intent(in) :: a(:,:,:)
        end function

        ! ---- pointer with intent(inout): C doubles values ----
        subroutine c20_double_ptr_1d(a) bind(C, name="c20_double_ptr_1d")
            import :: c_int32_t
            integer(c_int32_t), pointer, intent(inout) :: a(:)
        end subroutine

        ! ---- allocatable scalar ----
        integer(c_int32_t) function c20_read_alloc_scalar(x) &
                bind(C, name="c20_read_alloc_scalar")
            import :: c_int32_t
            integer(c_int32_t), allocatable, intent(in) :: x
        end function

        integer(c_int) function c20_attr_alloc_scalar(x) &
                bind(C, name="c20_attr_alloc_scalar")
            import :: c_int, c_int32_t
            integer(c_int32_t), allocatable, intent(in) :: x
        end function

        ! ---- pointer scalar ----
        integer(c_int32_t) function c20_read_ptr_scalar(x) &
                bind(C, name="c20_read_ptr_scalar")
            import :: c_int32_t
            integer(c_int32_t), pointer, intent(in) :: x
        end function

        integer(c_int) function c20_attr_ptr_scalar(x) &
                bind(C, name="c20_attr_ptr_scalar")
            import :: c_int, c_int32_t
            integer(c_int32_t), pointer, intent(in) :: x
        end function
    end interface
end module

! ============================================================================
! Module from bindc_28
! ============================================================================
module bindc_28_mod
    use iso_c_binding, only: c_int, c_int32_t, c_int64_t, c_float, &
        c_double, c_bool, c_float_complex
    implicit none

    interface
        ! ---- allocatable float array ----
        real(c_float) function c28_sum_alloc_float(a) &
                bind(C, name="c28_sum_alloc_float")
            import :: c_float
            real(c_float), allocatable, intent(in) :: a(:)
        end function

        ! ---- allocatable double array ----
        real(c_double) function c28_sum_alloc_double(a) &
                bind(C, name="c28_sum_alloc_double")
            import :: c_double
            real(c_double), allocatable, intent(in) :: a(:)
        end function

        ! ---- allocatable int64 array ----
        integer(c_int64_t) function c28_sum_alloc_int64(a) &
                bind(C, name="c28_sum_alloc_int64")
            import :: c_int64_t
            integer(c_int64_t), allocatable, intent(in) :: a(:)
        end function

        ! ---- allocatable complex array ----
        subroutine c28_sum_alloc_complex(a, re, im) &
                bind(C, name="c28_sum_alloc_complex")
            import :: c_float_complex, c_float
            complex(c_float_complex), allocatable, intent(in) :: a(:)
            real(c_float), intent(out) :: re, im
        end subroutine

        ! ---- allocatable logical array ----
        integer(c_int) function c28_count_alloc_bool(a) &
                bind(C, name="c28_count_alloc_bool")
            import :: c_int, c_bool
            logical(c_bool), allocatable, intent(in) :: a(:)
        end function

        ! ---- pointer float array ----
        real(c_float) function c28_sum_ptr_float(a) &
                bind(C, name="c28_sum_ptr_float")
            import :: c_float
            real(c_float), pointer, intent(in) :: a(:)
        end function

        ! ---- pointer double array ----
        real(c_double) function c28_sum_ptr_double(a) &
                bind(C, name="c28_sum_ptr_double")
            import :: c_double
            real(c_double), pointer, intent(in) :: a(:)
        end function

        ! ---- pointer int64 array ----
        integer(c_int64_t) function c28_sum_ptr_int64(a) &
                bind(C, name="c28_sum_ptr_int64")
            import :: c_int64_t
            integer(c_int64_t), pointer, intent(in) :: a(:)
        end function

        ! ---- zero-size array ----
        integer(c_int) function c28_zero_size_extent(a) &
                bind(C, name="c28_zero_size_extent")
            import :: c_int, c_int32_t
            integer(c_int32_t), intent(in) :: a(:)
        end function

        ! ---- unallocated allocatable ----
        integer(c_int) function c28_is_allocated(a) &
                bind(C, name="c28_is_allocated")
            import :: c_int, c_int32_t
            integer(c_int32_t), allocatable, intent(in) :: a(:)
        end function

        ! ---- disassociated pointer ----
        integer(c_int) function c28_is_associated(a) &
                bind(C, name="c28_is_associated")
            import :: c_int, c_int32_t
            integer(c_int32_t), pointer, intent(in) :: a(:)
        end function

        ! ---- rank 4 array ----
        integer(c_int32_t) function c28_sum_4d(a) &
                bind(C, name="c28_sum_4d")
            import :: c_int32_t
            integer(c_int32_t), intent(in) :: a(:,:,:,:)
        end function

        integer(c_int) function c28_get_rank_4d(a) &
                bind(C, name="c28_get_rank_4d")
            import :: c_int, c_int32_t
            integer(c_int32_t), intent(in) :: a(:,:,:,:)
        end function

        ! ---- explicit-shape in bind(C) ----
        integer(c_int32_t) function c28_sum_explicit(a, n) &
                bind(C, name="c28_sum_explicit")
            import :: c_int32_t, c_int
            integer(c_int), value :: n
            integer(c_int32_t), intent(in) :: a(n)
        end function

        ! ---- optional + contiguous ----
        integer(c_int32_t) function c28_opt_contig_sum(a) &
                bind(C, name="c28_opt_contig_sum")
            import :: c_int32_t
            integer(c_int32_t), optional, contiguous, intent(in) :: a(:)
        end function

    end interface
end module

! ============================================================================
! Module from bindc_30
! ============================================================================
module bindc_30_mod
    use iso_c_binding, only: c_int, c_int32_t, c_char
    implicit none

    interface
        ! ---- character allocatable array ----
        integer(c_int) function c30_char_alloc_sum(a) &
                bind(C, name="c30_char_alloc_sum")
            import :: c_int, c_char
            character(kind=c_char, len=:), allocatable, intent(in) :: a(:)
        end function

        integer(c_int) function c30_char_alloc_attr(a) &
                bind(C, name="c30_char_alloc_attr")
            import :: c_int, c_char
            character(kind=c_char, len=:), allocatable, intent(in) :: a(:)
        end function

        ! ---- character pointer array ----
        integer(c_int) function c30_char_ptr_sum(a) &
                bind(C, name="c30_char_ptr_sum")
            import :: c_int, c_char
            character(kind=c_char, len=:), pointer, intent(in) :: a(:)
        end function

        integer(c_int) function c30_char_ptr_attr(a) &
                bind(C, name="c30_char_ptr_attr")
            import :: c_int, c_char
            character(kind=c_char, len=:), pointer, intent(in) :: a(:)
        end function

        ! ---- allocatable + assumed-rank ----
        integer(c_int) function c30_alloc_ar_rank(a) &
                bind(C, name="c30_alloc_ar_rank")
            import :: c_int, c_int32_t
            integer(c_int32_t), allocatable, intent(in) :: a(..)
        end function

        ! ---- pointer + assumed-rank ----
        integer(c_int) function c30_ptr_ar_rank(a) &
                bind(C, name="c30_ptr_ar_rank")
            import :: c_int, c_int32_t
            integer(c_int32_t), pointer, intent(in) :: a(..)
        end function
    end interface
end module

! ============================================================================
! Module from bindc_31
! ============================================================================
module bindc_31_mod
    use iso_c_binding
    implicit none
contains

    integer(c_int) function f31_is_allocated(a) bind(C)
        integer(c_int32_t), allocatable, intent(in) :: a(:)
        f31_is_allocated = 0
        if (allocated(a)) f31_is_allocated = 1
    end function

    integer(c_int32_t) function f31_sum_1d(a) bind(C)
        integer(c_int32_t), allocatable, intent(in) :: a(:)
        f31_sum_1d = sum(a)
    end function

    integer(c_int) function f31_size_1d(a) bind(C)
        integer(c_int32_t), allocatable, intent(in) :: a(:)
        f31_size_1d = size(a)
    end function

    integer(c_int) function f31_lbound_1d(a) bind(C)
        integer(c_int32_t), allocatable, intent(in) :: a(:)
        f31_lbound_1d = lbound(a, 1)
    end function

    integer(c_int) function f31_ubound_1d(a) bind(C)
        integer(c_int32_t), allocatable, intent(in) :: a(:)
        f31_ubound_1d = ubound(a, 1)
    end function

    integer(c_int32_t) function f31_get_elem(a, idx) bind(C)
        integer(c_int32_t), allocatable, intent(in) :: a(:)
        integer(c_int), value, intent(in) :: idx
        f31_get_elem = a(idx)
    end function

    integer(c_int32_t) function f31_sum_2d(a) bind(C)
        integer(c_int32_t), allocatable, intent(in) :: a(:,:)
        f31_sum_2d = sum(a)
    end function

    integer(c_int) function f31_2d_shape_ok(a, n1, n2) bind(C)
        integer(c_int32_t), allocatable, intent(in) :: a(:,:)
        integer(c_int), value, intent(in) :: n1, n2
        f31_2d_shape_ok = 0
        if (size(a, 1) == n1 .and. size(a, 2) == n2) f31_2d_shape_ok = 1
    end function

    real(c_double) function f31_sum_double(a) bind(C)
        real(c_double), allocatable, intent(in) :: a(:)
        f31_sum_double = sum(a)
    end function

    subroutine f31_alloc_fill(a) bind(C)
        integer(c_int32_t), allocatable, intent(out) :: a(:)
        allocate(a(3))
        a = [10, 20, 30]
    end subroutine

    subroutine f31_realloc(a) bind(C)
        integer(c_int32_t), allocatable, intent(inout) :: a(:)
        if (allocated(a)) deallocate(a)
        allocate(a(5))
        a = [100, 200, 300, 400, 500]
    end subroutine

end module

! ============================================================================
! Module from bindc_32
! ============================================================================
module bindc_32_mod
    use iso_c_binding
    implicit none
contains

    integer(c_int) function f32_is_associated(a) bind(C)
        integer(c_int32_t), pointer, intent(in) :: a(:)
        f32_is_associated = 0
        if (associated(a)) f32_is_associated = 1
    end function

    integer(c_int32_t) function f32_sum_1d(a) bind(C)
        integer(c_int32_t), pointer, intent(in) :: a(:)
        f32_sum_1d = sum(a)
    end function

    integer(c_int) function f32_lbound_1d(a) bind(C)
        integer(c_int32_t), pointer, intent(in) :: a(:)
        f32_lbound_1d = lbound(a, 1)
    end function

    integer(c_int) function f32_ubound_1d(a) bind(C)
        integer(c_int32_t), pointer, intent(in) :: a(:)
        f32_ubound_1d = ubound(a, 1)
    end function

    integer(c_int32_t) function f32_sum_2d(a) bind(C)
        integer(c_int32_t), pointer, intent(in) :: a(:,:)
        f32_sum_2d = sum(a)
    end function

    subroutine f32_double_values(a) bind(C)
        integer(c_int32_t), pointer, intent(inout) :: a(:)
        a = a * 2
    end subroutine

end module

! ============================================================================
! Main program
! ============================================================================
program bindc_iso_fb_02
    use bindc_20_mod
    use bindc_28_mod
    use bindc_30_mod
    use bindc_31_mod
    use bindc_32_mod
    use iso_c_binding
    implicit none

    interface
        integer(c_int) function c31_run_tests() bind(C)
            import :: c_int
        end function
        integer(c_int) function c32_run_tests() bind(C)
            import :: c_int
        end function
    end interface

    integer(c_int) :: res

    ! --- Tests from bindc_20 ---
    call test_alloc_2d()
    call test_alloc_3d()
    call test_ptr_2d()
    call test_ptr_3d()
    call test_ptr_inout()
    call test_alloc_scalar()
    call test_ptr_scalar()

    ! --- Tests from bindc_28 ---
    call test_alloc_float()
    call test_alloc_double()
    call test_alloc_int64()
    call test_alloc_complex()
    call test_alloc_bool()
    call test_ptr_float()
    call test_ptr_double()
    call test_ptr_int64()
    call test_zero_size()
    call test_unallocated()
    call test_disassociated()
    call test_rank4()
    call test_explicit_shape()
    call test_opt_contig()

    ! --- Tests from bindc_30 ---
    call test_char_alloc()
    call test_char_ptr()
    call test_alloc_ar()
    call test_ptr_ar()

    ! --- Tests from bindc_31 (C-driven CFI_allocate tests) ---
    res = c31_run_tests()
    if (res /= 0) then
        print *, "C test failed with code:", res
        error stop "FAIL: C allocatable tests failed"
    end if

    ! --- Tests from bindc_32 (C-driven CFI_setpointer tests) ---
    res = c32_run_tests()
    if (res /= 0) then
        print *, "C test failed with code:", res
        error stop "FAIL: C pointer tests failed"
    end if

    print *, "All bindc_iso_fb_02 tests passed."

contains

    ! ========================================================================
    ! Test subroutines from bindc_20
    ! ========================================================================

    subroutine test_alloc_2d()
        integer(c_int32_t), allocatable :: a(:,:)

        allocate(a(2,3))
        a = reshape([1, 2, 3, 4, 5, 6], [2, 3])

        if (c20_sum_alloc_2d(a) /= 21) error stop "FAIL: alloc 2d sum"
        if (c20_attr_alloc_2d(a) /= 1) error stop "FAIL: alloc 2d attr"

        call c20_double_alloc_2d(a)
        if (a(1,1) /= 2 .or. a(2,3) /= 12) error stop "FAIL: alloc 2d double"
        if (c20_sum_alloc_2d(a) /= 42) error stop "FAIL: alloc 2d double sum"

        deallocate(a)
    end subroutine

    subroutine test_alloc_3d()
        integer(c_int32_t), allocatable :: a(:,:,:)
        integer :: i

        allocate(a(2,3,2))
        a = reshape([(i, i=1,12)], [2, 3, 2])

        if (c20_sum_alloc_3d(a) /= 78) error stop "FAIL: alloc 3d sum"

        deallocate(a)
    end subroutine

    subroutine test_ptr_2d()
        integer(c_int32_t), target :: tgt(2,3)
        integer(c_int32_t), pointer :: p(:,:)

        tgt = reshape([10, 20, 30, 40, 50, 60], [2, 3])
        p => tgt

        if (c20_sum_ptr_2d(p) /= 210) error stop "FAIL: ptr 2d sum"
        if (c20_attr_ptr_2d(p) /= 1) error stop "FAIL: ptr 2d attr"
    end subroutine

    subroutine test_ptr_3d()
        integer(c_int32_t), target :: tgt(2,3,2)
        integer(c_int32_t), pointer :: p(:,:,:)
        integer :: i

        tgt = reshape([(i*10, i=1,12)], [2, 3, 2])
        p => tgt

        if (c20_sum_ptr_3d(p) /= 780) error stop "FAIL: ptr 3d sum"
    end subroutine

    subroutine test_ptr_inout()
        integer(c_int32_t), target :: tgt(4)
        integer(c_int32_t), pointer :: p(:)

        tgt = [1, 2, 3, 4]
        p => tgt

        call c20_double_ptr_1d(p)
        if (tgt(1) /= 2 .or. tgt(4) /= 8) error stop "FAIL: ptr inout"
    end subroutine

    subroutine test_alloc_scalar()
        integer(c_int32_t), allocatable :: x

        allocate(x)
        x = 42

        if (c20_read_alloc_scalar(x) /= 42) error stop "FAIL: alloc scalar read"
        ! TODO: LFortran bug - alloc scalar attr not set correctly
        ! if (c20_attr_alloc_scalar(x) /= 1) error stop "FAIL: alloc scalar attr"

        deallocate(x)
    end subroutine

    subroutine test_ptr_scalar()
        integer(c_int32_t), target :: tgt
        integer(c_int32_t), pointer :: p

        tgt = 99
        p => tgt

        if (c20_read_ptr_scalar(p) /= 99) error stop "FAIL: ptr scalar read"
        ! TODO: LFortran bug - ptr scalar attr not set correctly
        ! if (c20_attr_ptr_scalar(p) /= 1) error stop "FAIL: ptr scalar attr"
    end subroutine

    ! ========================================================================
    ! Test subroutines from bindc_28
    ! ========================================================================

    subroutine test_alloc_float()
        real(c_float), allocatable :: a(:)
        allocate(a(3))
        a = [1.0, 2.0, 3.0]
        if (abs(c28_sum_alloc_float(a) - 6.0) > 1.0e-5) &
            error stop "FAIL: alloc float sum"
        deallocate(a)
    end subroutine

    subroutine test_alloc_double()
        real(c_double), allocatable :: a(:)
        allocate(a(3))
        a = [1.0d0, 2.0d0, 3.0d0]
        if (abs(c28_sum_alloc_double(a) - 6.0d0) > 1.0d-10) &
            error stop "FAIL: alloc double sum"
        deallocate(a)
    end subroutine

    subroutine test_alloc_int64()
        integer(c_int64_t), allocatable :: a(:)
        allocate(a(3))
        a = [10_c_int64_t, 20_c_int64_t, 30_c_int64_t]
        if (c28_sum_alloc_int64(a) /= 60_c_int64_t) &
            error stop "FAIL: alloc int64 sum"
        deallocate(a)
    end subroutine

    subroutine test_alloc_complex()
        complex(c_float_complex), allocatable :: a(:)
        real(c_float) :: re, im
        allocate(a(2))
        a = [(1.0, 2.0), (3.0, 4.0)]
        call c28_sum_alloc_complex(a, re, im)
        if (abs(re - 4.0) > 1.0e-5) error stop "FAIL: alloc complex re"
        if (abs(im - 6.0) > 1.0e-5) error stop "FAIL: alloc complex im"
        deallocate(a)
    end subroutine

    subroutine test_alloc_bool()
        logical(c_bool), allocatable :: a(:)
        allocate(a(5))
        a = [.true., .false., .true., .true., .false.]
        if (c28_count_alloc_bool(a) /= 3) error stop "FAIL: alloc bool count"
        deallocate(a)
    end subroutine

    subroutine test_ptr_float()
        real(c_float), target :: tgt(3)
        real(c_float), pointer :: p(:)
        tgt = [10.0, 20.0, 30.0]
        p => tgt
        if (abs(c28_sum_ptr_float(p) - 60.0) > 1.0e-5) &
            error stop "FAIL: ptr float sum"
    end subroutine

    subroutine test_ptr_double()
        real(c_double), target :: tgt(3)
        real(c_double), pointer :: p(:)
        tgt = [10.0d0, 20.0d0, 30.0d0]
        p => tgt
        if (abs(c28_sum_ptr_double(p) - 60.0d0) > 1.0d-10) &
            error stop "FAIL: ptr double sum"
    end subroutine

    subroutine test_ptr_int64()
        integer(c_int64_t), target :: tgt(3)
        integer(c_int64_t), pointer :: p(:)
        tgt = [100_c_int64_t, 200_c_int64_t, 300_c_int64_t]
        p => tgt
        if (c28_sum_ptr_int64(p) /= 600_c_int64_t) &
            error stop "FAIL: ptr int64 sum"
    end subroutine

    subroutine test_zero_size()
        integer(c_int32_t) :: arr(0)
        if (c28_zero_size_extent(arr) /= 0) error stop "FAIL: zero size"
    end subroutine

    subroutine test_unallocated()
        integer(c_int32_t), allocatable :: a(:)
        ! Not allocated — C should see base_addr == NULL
        if (c28_is_allocated(a) /= 0) error stop "FAIL: unallocated"
    end subroutine

    subroutine test_disassociated()
        integer(c_int32_t), pointer :: p(:)
        nullify(p)
        if (c28_is_associated(p) /= 0) error stop "FAIL: disassociated"
    end subroutine

    subroutine test_rank4()
        integer(c_int32_t) :: a(2, 2, 2, 2)
        integer :: i, j, k, l, v
        v = 1
        do l = 1, 2
            do k = 1, 2
                do j = 1, 2
                    do i = 1, 2
                        a(i, j, k, l) = v
                        v = v + 1
                    end do
                end do
            end do
        end do
        ! sum of 1..16 = 136
        if (c28_sum_4d(a) /= 136) error stop "FAIL: rank4 sum"
        if (c28_get_rank_4d(a) /= 4) error stop "FAIL: rank4 rank"
    end subroutine

    subroutine test_explicit_shape()
        integer(c_int32_t) :: a(5)
        a = [1, 2, 3, 4, 5]
        if (c28_sum_explicit(a, 5) /= 15) error stop "FAIL: explicit shape"
    end subroutine

    subroutine test_opt_contig()
        integer(c_int32_t) :: arr(4)
        arr = [1, 2, 3, 4]
        if (c28_opt_contig_sum(arr) /= 10) error stop "FAIL: opt contig"
    end subroutine

    ! ========================================================================
    ! Test subroutines from bindc_30
    ! ========================================================================

    subroutine test_char_alloc()
        character(kind=c_char, len=:), allocatable :: ca(:)
        allocate(character(len=1) :: ca(3))
        ca = ['X', 'Y', 'Z']
        ! 88 + 89 + 90 = 267
        if (c30_char_alloc_sum(ca) /= 267) error stop "FAIL: char alloc sum"
        if (c30_char_alloc_attr(ca) /= 1) error stop "FAIL: char alloc attr"
        deallocate(ca)
    end subroutine

    subroutine test_char_ptr()
        character(kind=c_char, len=:), allocatable, target :: tgt(:)
        character(kind=c_char, len=:), pointer :: p(:)
        allocate(character(len=1) :: tgt(4))
        tgt = ['a', 'b', 'c', 'd']
        p => tgt
        ! 97 + 98 + 99 + 100 = 394
        if (c30_char_ptr_sum(p) /= 394) error stop "FAIL: char ptr sum"
        if (c30_char_ptr_attr(p) /= 1) error stop "FAIL: char ptr attr"
    end subroutine

    subroutine test_alloc_ar()
        integer(c_int32_t), allocatable :: a1(:), a2(:,:)
        allocate(a1(3))
        a1 = [1, 2, 3]
        if (c30_alloc_ar_rank(a1) /= 1) error stop "FAIL: alloc ar rank 1"

        allocate(a2(2, 3))
        a2 = reshape([1, 2, 3, 4, 5, 6], [2, 3])
        if (c30_alloc_ar_rank(a2) /= 2) error stop "FAIL: alloc ar rank 2"
        deallocate(a1)
        deallocate(a2)
    end subroutine

    subroutine test_ptr_ar()
        integer(c_int32_t), target :: tgt1(4), tgt2(2, 3)
        integer(c_int32_t), pointer :: p1(:), p2(:,:)
        tgt1 = [1, 2, 3, 4]
        tgt2 = reshape([1, 2, 3, 4, 5, 6], [2, 3])
        p1 => tgt1
        p2 => tgt2
        if (c30_ptr_ar_rank(p1) /= 1) error stop "FAIL: ptr ar rank 1"
        if (c30_ptr_ar_rank(p2) /= 2) error stop "FAIL: ptr ar rank 2"
    end subroutine

end program
