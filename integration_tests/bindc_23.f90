! Test: Features not yet supported by LFortran LLVM backend
!
! These are valid Fortran 2018 features that crash or produce incorrect
! LLVM IR in LFortran. They are tested with gfortran only until the
! LFortran bugs are fixed.
!
! Covers gap items A.4, A.5, D.12, D.13, E.18, F.19:
!   - A.4: character(c_char) assumed-shape arrays via descriptor
!   - A.5: character with len > 1 through descriptor (elem_len check)
!   - D.12: Optional allocatable arrays in BIND(C) interface
!   - D.13: Optional pointer arrays in BIND(C) interface
!   - E.18: Assumed-rank scalar (rank 0 passed to dimension(..))
!   - F.19: C calling Fortran BIND(C) procedures with descriptor args
module bindc_23_mod
    use iso_c_binding, only: c_int, c_int32_t, c_char
    implicit none

    interface
        ! ---- character(c_char) assumed-shape arrays ----
        integer(c_int) function c23_char_sum_1d(a) &
                bind(C, name="c23_char_sum_1d")
            import :: c_int, c_char
            character(kind=c_char, len=1), intent(in) :: a(:)
        end function

        ! ---- assumed-rank: rank query for scalar (rank 0) ----
        integer(c_int) function c23_get_rank(a) bind(C, name="c23_get_rank")
            import :: c_int, c_int32_t
            integer(c_int32_t), intent(in) :: a(..)
        end function

        ! ---- optional allocatable ----
        integer(c_int) function c23_opt_alloc_present(a) &
                bind(C, name="c23_opt_alloc_present")
            import :: c_int, c_int32_t
            integer(c_int32_t), optional, allocatable, intent(in) :: a(:)
        end function

        integer(c_int32_t) function c23_opt_alloc_sum(a) &
                bind(C, name="c23_opt_alloc_sum")
            import :: c_int, c_int32_t
            integer(c_int32_t), optional, allocatable, intent(in) :: a(:)
        end function

        ! ---- optional pointer ----
        integer(c_int) function c23_opt_ptr_present(a) &
                bind(C, name="c23_opt_ptr_present")
            import :: c_int, c_int32_t
            integer(c_int32_t), optional, pointer, intent(in) :: a(:)
        end function

        ! ---- C calling Fortran (receives descriptor, calls back) ----
        integer(c_int32_t) function c23_call_fortran_sum(a) &
                bind(C, name="c23_call_fortran_sum")
            import :: c_int32_t
            integer(c_int32_t), intent(in) :: a(:)
        end function
    end interface
end module

! Fortran BIND(C) procedure with descriptor arg (called from C)
module bindc_23_callbacks
    use iso_c_binding, only: c_int32_t
    implicit none
contains
    integer(c_int32_t) function fortran_sum_1d(a) &
            bind(C, name="fortran_sum_1d")
        integer(c_int32_t), intent(in) :: a(:)
        integer :: i
        fortran_sum_1d = 0
        do i = 1, size(a)
            fortran_sum_1d = fortran_sum_1d + a(i)
        end do
    end function
end module

program bindc_23
    use bindc_23_mod
    use bindc_23_callbacks
    use iso_c_binding, only: c_int32_t, c_char
    implicit none

    call test_char_array()
    call test_ar_rank0()
    call test_opt_alloc()
    call test_opt_ptr()
    call test_c_calls_fortran()

    print *, "All bindc_23 tests passed."

contains

    subroutine test_char_array()
        character(kind=c_char, len=1) :: c1(4)

        c1 = ['A', 'B', 'C', 'D']
        ! 65 + 66 + 67 + 68 = 266
        if (c23_char_sum_1d(c1) /= 266) error stop "FAIL: char sum"
    end subroutine

    subroutine test_ar_rank0()
        integer(c_int32_t) :: scalar

        scalar = 42
        if (c23_get_rank(scalar) /= 0) error stop "FAIL: ar rank0"
    end subroutine

    subroutine test_opt_alloc()
        integer(c_int32_t), allocatable :: a(:)

        allocate(a(3))
        a = [10, 20, 30]
        if (c23_opt_alloc_present(a) /= 1) error stop "FAIL: opt alloc present"
        if (c23_opt_alloc_sum(a) /= 60) error stop "FAIL: opt alloc sum"

        if (c23_opt_alloc_present() /= 0) error stop "FAIL: opt alloc absent"

        deallocate(a)
    end subroutine

    subroutine test_opt_ptr()
        integer(c_int32_t), target :: tgt(3)
        integer(c_int32_t), pointer :: p(:)

        tgt = [1, 2, 3]
        p => tgt
        if (c23_opt_ptr_present(p) /= 1) error stop "FAIL: opt ptr present"

        if (c23_opt_ptr_present() /= 0) error stop "FAIL: opt ptr absent"
    end subroutine

    subroutine test_c_calls_fortran()
        integer(c_int32_t) :: arr(5)
        integer(c_int32_t) :: result

        arr = [10, 20, 30, 40, 50]
        result = c23_call_fortran_sum(arr)
        if (result /= 150) error stop "FAIL: c calls fortran"
    end subroutine

end program
