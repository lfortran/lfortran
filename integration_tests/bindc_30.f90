! Test: Features not yet supported by LFortran LLVM backend (part 2)
!
! These are valid Fortran 2018 features that are not yet handled by
! LFortran. They are tested with gfortran/flang only.
!
! Covers:
!   - Allocatable character arrays via descriptor
!   - Pointer character arrays via descriptor
!   - Allocatable + assumed-rank
!   - Pointer + assumed-rank
module bindc_30_mod
    use iso_c_binding, only: c_int, c_int32_t, c_char
    implicit none

    interface
        ! ---- character allocatable array ----
        integer(c_int) function c30_char_alloc_sum(a) &
                bind(C, name="c30_char_alloc_sum")
            import :: c_int, c_char
            character(kind=c_char, len=1), allocatable, intent(in) :: a(:)
        end function

        integer(c_int) function c30_char_alloc_attr(a) &
                bind(C, name="c30_char_alloc_attr")
            import :: c_int, c_char
            character(kind=c_char, len=1), allocatable, intent(in) :: a(:)
        end function

        ! ---- character pointer array ----
        integer(c_int) function c30_char_ptr_sum(a) &
                bind(C, name="c30_char_ptr_sum")
            import :: c_int, c_char
            character(kind=c_char, len=1), pointer, intent(in) :: a(:)
        end function

        integer(c_int) function c30_char_ptr_attr(a) &
                bind(C, name="c30_char_ptr_attr")
            import :: c_int, c_char
            character(kind=c_char, len=1), pointer, intent(in) :: a(:)
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

program bindc_30
    use bindc_30_mod
    use iso_c_binding, only: c_int32_t, c_char
    implicit none

    call test_char_alloc()
    call test_char_ptr()
    call test_alloc_ar()
    call test_ptr_ar()

    print *, "All bindc_30 tests passed."

contains

    subroutine test_char_alloc()
        character(kind=c_char, len=1), allocatable :: ca(:)
        allocate(ca(3))
        ca = ['X', 'Y', 'Z']
        ! 88 + 89 + 90 = 267
        if (c30_char_alloc_sum(ca) /= 267) error stop "FAIL: char alloc sum"
        if (c30_char_alloc_attr(ca) /= 1) error stop "FAIL: char alloc attr"
        deallocate(ca)
    end subroutine

    subroutine test_char_ptr()
        character(kind=c_char, len=1), target :: tgt(4)
        character(kind=c_char, len=1), pointer :: p(:)
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
