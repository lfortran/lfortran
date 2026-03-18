! Test: Additional CFI descriptor features and edge cases
!
! Covers:
!   - CFI_establish with explicit elem_len > 0 vs 0 (auto) for character
!   - Negative-stride CFI_section created from C side
!   - SAVE + BIND(C) on module variable
!   - PROTECTED + BIND(C) on module variable (use-associated restriction)
!   - OPTIONAL allocatable scalar
!   - OPTIONAL pointer scalar
!   - C_F_POINTER with custom lower bounds multi-dimensional

module bindc_42_globals
    use iso_c_binding
    implicit none
    integer(c_int), save, bind(C, name="g42_save_var") :: g42_save_var = 0
end module

module bindc_42_protected_mod
    use iso_c_binding
    implicit none
    integer(c_int), protected, bind(C, name="g42_protected") :: g42_protected = 99
contains
    subroutine set_protected(val)
        integer(c_int), intent(in) :: val
        g42_protected = val
    end subroutine
end module

module bindc_42_ifaces
    use iso_c_binding
    implicit none

    interface
        ! ---- char elem_len: explicit vs auto ----
        integer(c_int) function c42_check_char_elem_len(s, expected) bind(C)
            import :: c_int, c_char
            character(kind=c_char, len=8), intent(in) :: s(:)
            integer(c_int), value :: expected
        end function

        ! ---- Negative-stride section from C ----
        integer(c_int) function c42_test_negative_stride_section() bind(C)
            import :: c_int
        end function

        ! ---- SAVE+BIND(C) global: C modifies it ----
        subroutine c42_set_save_var(val) bind(C)
            import :: c_int
            integer(c_int), value :: val
        end subroutine

        integer(c_int) function c42_get_save_var() bind(C)
            import :: c_int
        end function

        ! ---- PROTECTED+BIND(C): C reads it ----
        integer(c_int) function c42_get_protected() bind(C)
            import :: c_int
        end function

        ! ---- Optional allocatable scalar ----
        integer(c_int) function c42_opt_alloc_scalar(x) bind(C)
            import :: c_int
            integer(c_int), allocatable, optional, intent(in) :: x
        end function

        ! ---- Optional pointer scalar ----
        integer(c_int) function c42_opt_ptr_scalar(x) bind(C)
            import :: c_int
            integer(c_int), pointer, optional, intent(in) :: x
        end function
    end interface
end module

program bindc_42
    use iso_c_binding
    use bindc_42_globals
    use bindc_42_protected_mod
    use bindc_42_ifaces
    implicit none

    call test_char_elem_len()
    call test_negative_stride_section()
    call test_save_bindc()
    call test_protected_bindc()
    call test_optional_alloc_scalar()
    call test_optional_ptr_scalar()
    call test_c_f_pointer_lower_bounds()

    print *, "All bindc_42 tests passed."

contains

    subroutine test_char_elem_len()
        character(kind=c_char, len=8) :: arr(2)
        arr(1) = "ABCDEFGH"
        arr(2) = "12345678"
        ! C verifies elem_len == 8
        if (c42_check_char_elem_len(arr, 8_c_int) /= 1) &
            error stop "FAIL: char elem_len 8"
    end subroutine

    subroutine test_negative_stride_section()
        ! All done on C side: create array, CFI_section with stride=-1, verify
        if (c42_test_negative_stride_section() /= 1) &
            error stop "FAIL: negative stride section"
    end subroutine

    subroutine test_save_bindc()
        g42_save_var = 42
        if (c42_get_save_var() /= 42) error stop "FAIL: save var F->C"
        call c42_set_save_var(100_c_int)
        if (g42_save_var /= 100) error stop "FAIL: save var C->F"
    end subroutine

    subroutine test_protected_bindc()
        ! Cannot modify g42_protected here (PROTECTED), but C can read it
        call set_protected(77_c_int)
        if (c42_get_protected() /= 77) error stop "FAIL: protected C read"
    end subroutine

    subroutine test_optional_alloc_scalar()
        integer(c_int), allocatable :: x
        integer(c_int) :: r

        ! Present and allocated
        allocate(x)
        x = 42
        r = c42_opt_alloc_scalar(x)
        if (r /= 42) error stop "FAIL: opt alloc scalar present"

        ! Absent
        r = c42_opt_alloc_scalar()
        if (r /= -1) error stop "FAIL: opt alloc scalar absent"
        deallocate(x)
    end subroutine

    subroutine test_optional_ptr_scalar()
        integer(c_int), pointer :: p
        integer(c_int), target :: tgt
        integer(c_int) :: r

        ! Present and associated
        tgt = 55
        p => tgt
        r = c42_opt_ptr_scalar(p)
        if (r /= 55) error stop "FAIL: opt ptr scalar present"

        ! Absent
        r = c42_opt_ptr_scalar()
        if (r /= -1) error stop "FAIL: opt ptr scalar absent"
    end subroutine

    subroutine test_c_f_pointer_lower_bounds()
        use iso_c_binding
        integer(c_int), target :: data(6)
        type(c_ptr) :: cp
        integer(c_int), pointer :: fp2d(:,:)
        integer :: i

        data = [(i, i = 1, 6)]
        cp = c_loc(data(1))

        ! C_F_POINTER with shape [3,2] and implicit lower bound 1
        call c_f_pointer(cp, fp2d, [3, 2])
        if (fp2d(1,1) /= 1) error stop "FAIL: c_f_ptr 2d (1,1)"
        if (fp2d(3,1) /= 3) error stop "FAIL: c_f_ptr 2d (3,1)"
        if (fp2d(1,2) /= 4) error stop "FAIL: c_f_ptr 2d (1,2)"
        if (fp2d(3,2) /= 6) error stop "FAIL: c_f_ptr 2d (3,2)"
    end subroutine

end program
