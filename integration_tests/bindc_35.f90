! Test: ISO_C_BINDING named constants completeness
!
! Covers:
!   - Character named constants: C_ALERT, C_BACKSPACE, C_FORM_FEED,
!     C_CARRIAGE_RETURN, C_HORIZONTAL_TAB, C_VERTICAL_TAB
!   - Integer kind constants: C_INTMAX_T
!   - Integer kind constants: C_INT_LEAST8_T .. C_INT_LEAST64_T
!   - Integer kind constants: C_INT_FAST8_T  .. C_INT_FAST64_T
program bindc_35
    use, intrinsic :: iso_c_binding
    implicit none

    call test_char_constants()
    call test_intmax()
    call test_int_least()
    call test_int_fast()

    print *, "All bindc_35 tests passed."

contains

    subroutine test_char_constants()
        ! Verify ASCII values of all character named constants
        if (iachar(c_null_char) /= 0) error stop "FAIL: C_NULL_CHAR"
        if (iachar(c_alert) /= 7) error stop "FAIL: C_ALERT"
        if (iachar(c_backspace) /= 8) error stop "FAIL: C_BACKSPACE"
        if (iachar(c_horizontal_tab) /= 9) error stop "FAIL: C_HORIZONTAL_TAB"
        if (iachar(c_new_line) /= 10) error stop "FAIL: C_NEW_LINE"
        if (iachar(c_vertical_tab) /= 11) error stop "FAIL: C_VERTICAL_TAB"
        if (iachar(c_form_feed) /= 12) error stop "FAIL: C_FORM_FEED"
        if (iachar(c_carriage_return) /= 13) error stop "FAIL: C_CARRIAGE_RETURN"
    end subroutine

    subroutine test_intmax()
        interface
            integer(c_intmax_t) function c35_double_intmax(x) bind(c)
                import :: c_intmax_t
                integer(c_intmax_t), value :: x
            end function
        end interface
        integer(c_intmax_t) :: v, r
        v = 42
        r = c35_double_intmax(v)
        if (r /= 84) error stop "FAIL: C_INTMAX_T"
    end subroutine

    subroutine test_int_least()
        interface
            integer(c_int_least8_t) function c35_double_least8(x) bind(c)
                import :: c_int_least8_t
                integer(c_int_least8_t), value :: x
            end function
            integer(c_int_least16_t) function c35_double_least16(x) bind(c)
                import :: c_int_least16_t
                integer(c_int_least16_t), value :: x
            end function
            integer(c_int_least32_t) function c35_double_least32(x) bind(c)
                import :: c_int_least32_t
                integer(c_int_least32_t), value :: x
            end function
            integer(c_int_least64_t) function c35_double_least64(x) bind(c)
                import :: c_int_least64_t
                integer(c_int_least64_t), value :: x
            end function
        end interface
        if (c35_double_least8(10_c_int_least8_t) /= 20) &
            error stop "FAIL: C_INT_LEAST8_T"
        if (c35_double_least16(100_c_int_least16_t) /= 200) &
            error stop "FAIL: C_INT_LEAST16_T"
        if (c35_double_least32(1000_c_int_least32_t) /= 2000) &
            error stop "FAIL: C_INT_LEAST32_T"
        if (c35_double_least64(10000_c_int_least64_t) /= 20000) &
            error stop "FAIL: C_INT_LEAST64_T"
    end subroutine

    subroutine test_int_fast()
        interface
            integer(c_int_fast8_t) function c35_double_fast8(x) bind(c)
                import :: c_int_fast8_t
                integer(c_int_fast8_t), value :: x
            end function
            integer(c_int_fast16_t) function c35_double_fast16(x) bind(c)
                import :: c_int_fast16_t
                integer(c_int_fast16_t), value :: x
            end function
            integer(c_int_fast32_t) function c35_double_fast32(x) bind(c)
                import :: c_int_fast32_t
                integer(c_int_fast32_t), value :: x
            end function
            integer(c_int_fast64_t) function c35_double_fast64(x) bind(c)
                import :: c_int_fast64_t
                integer(c_int_fast64_t), value :: x
            end function
        end interface
        if (c35_double_fast8(5_c_int_fast8_t) /= 10) &
            error stop "FAIL: C_INT_FAST8_T"
        if (c35_double_fast16(50_c_int_fast16_t) /= 100) &
            error stop "FAIL: C_INT_FAST16_T"
        if (c35_double_fast32(500_c_int_fast32_t) /= 1000) &
            error stop "FAIL: C_INT_FAST32_T"
        if (c35_double_fast64(5000_c_int_fast64_t) /= 10000) &
            error stop "FAIL: C_INT_FAST64_T"
    end subroutine

end program
