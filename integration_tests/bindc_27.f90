! Test: Character interop through descriptors
!
! Covers gap items B.2-B.6:
!   - character(c_char, len=1) 1D/2D arrays through descriptor
!   - character array sections through descriptor
!   - character inout (modify in C)
!
! Allocatable/pointer character arrays moved to bindc_30 (gfortran-only)
module bindc_27_mod
    use iso_c_binding, only: c_int, c_char, c_int32_t
    implicit none

    interface
        ! ---- character(len=1) 1D sum ----
        integer(c_int) function c27_char_sum_1d(a) &
                bind(C, name="c27_char_sum_1d")
            import :: c_int, c_char
            character(kind=c_char, len=1), intent(in) :: a(:)
        end function

        ! ---- character(len=1) 2D sum ----
        integer(c_int) function c27_char_sum_2d(a) &
                bind(C, name="c27_char_sum_2d")
            import :: c_int, c_char
            character(kind=c_char, len=1), intent(in) :: a(:,:)
        end function

        ! ---- character inout (modify in C) ----
        subroutine c27_char_toupper(a) bind(C, name="c27_char_toupper")
            import :: c_char
            character(kind=c_char, len=1), intent(inout) :: a(:)
        end subroutine
    end interface
end module

program bindc_27
    use bindc_27_mod
    use iso_c_binding, only: c_char
    implicit none

    call test_char_1d()
    call test_char_2d()
    call test_char_section()
    call test_char_inout()

    print *, "All bindc_27 tests passed."

contains

    subroutine test_char_1d()
        character(kind=c_char, len=1) :: c1(4)
        c1 = ['A', 'B', 'C', 'D']
        ! 65 + 66 + 67 + 68 = 266
        if (c27_char_sum_1d(c1) /= 266) error stop "FAIL: char 1d sum"
    end subroutine

    subroutine test_char_2d()
        character(kind=c_char, len=1) :: c2(2, 3)
        c2(1, 1) = 'A'
        c2(2, 1) = 'B'
        c2(1, 2) = 'C'
        c2(2, 2) = 'D'
        c2(1, 3) = 'E'
        c2(2, 3) = 'F'
        ! 65+66+67+68+69+70 = 405
        if (c27_char_sum_2d(c2) /= 405) error stop "FAIL: char 2d sum"
    end subroutine

    subroutine test_char_section()
        character(kind=c_char, len=1) :: arr(6)
        arr = ['A', 'B', 'C', 'D', 'E', 'F']
        ! stride-2: A, C, E = 65 + 67 + 69 = 201
        if (c27_char_sum_1d(arr(1::2)) /= 201) &
            error stop "FAIL: char section"
    end subroutine

    subroutine test_char_inout()
        character(kind=c_char, len=1) :: arr(3)
        arr = ['a', 'b', 'c']
        call c27_char_toupper(arr)
        if (arr(1) /= 'A') error stop "FAIL: toupper 1"
        if (arr(2) /= 'B') error stop "FAIL: toupper 2"
        if (arr(3) /= 'C') error stop "FAIL: toupper 3"
    end subroutine

end program
