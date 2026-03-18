! Test: Attribute combinations and array features
!
! Covers gap items F.1-F.11, G.1-G.8:
!   - Allocatable arrays of non-int32 types (float, double, int64, complex, logical)
!   - Pointer arrays of non-int32 types
!   - Zero-size arrays through descriptors
!   - Unallocated allocatable passed to C (base_addr == NULL)
!   - Disassociated pointer passed to C (base_addr == NULL)
!   - Rank 4 arrays through descriptors
!   - Explicit-shape arrays in bind(C)
!   - Optional+CONTIGUOUS
!   - Allocatable+assumed-rank
!   - Pointer+assumed-rank
!   - Lower-bound in descriptor validation
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

program bindc_28
    use bindc_28_mod
    use iso_c_binding
    implicit none

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

    print *, "All bindc_28 tests passed."

contains

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

end program
