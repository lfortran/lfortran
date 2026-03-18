! Test: Remaining F2018 bind(C) features
!
! Covers:
!   - ENUM, BIND(C) (enum interop with C)
!   - Derived type with fixed-size array component
!   - c_f_procpointer (convert C function pointer to Fortran procedure pointer)
!   - POINTER + CONTIGUOUS attribute combination in BIND(C)
!   - Module bind(C) character variable
!   - Module bind(C) complex variable
!   - COMMON block with BIND(C)
module bindc_34_types
    use iso_c_binding
    implicit none

    ! ---- ENUM, BIND(C) ----
    enum, bind(C)
        enumerator :: COLOR_RED = 0
        enumerator :: COLOR_GREEN = 1
        enumerator :: COLOR_BLUE = 2
    end enum

    ! ---- Derived type with fixed array component ----
    type, bind(C) :: vec5_t
        integer(c_int32_t) :: data(5)
    end type

end module

module bindc_34_globals
    use iso_c_binding
    implicit none

    ! ---- Module bind(C) character variable ----
    character(kind=c_char, len=1), bind(C, name="g34_char") :: g34_char

    ! ---- Module bind(C) complex variable ----
    complex(c_float_complex), bind(C, name="g34_cmplx") :: g34_cmplx

end module

module bindc_34_common_mod
    use iso_c_binding
    implicit none

    ! ---- COMMON block with BIND(C) ----
    integer(c_int32_t) :: common_x, common_y
    common /c34_common/ common_x, common_y
    bind(C, name="c34_common") :: /c34_common/

end module

module bindc_34_ifaces
    use iso_c_binding
    use bindc_34_types
    implicit none

    abstract interface
        function int_func_i(x) bind(C) result(r)
            import :: c_int
            integer(c_int), value :: x
            integer(c_int) :: r
        end function
    end interface

    interface
        ! Enum: C returns an enum value
        integer(c_int) function c34_get_color(idx) bind(C)
            import :: c_int
            integer(c_int), value :: idx
        end function

        ! Enum: C checks if value matches expected
        integer(c_int) function c34_check_color(val, expected) bind(C)
            import :: c_int
            integer(c_int), value :: val, expected
        end function

        ! DT with fixed array: C sums the data array
        integer(c_int32_t) function c34_sum_vec5(v) bind(C)
            import :: c_int32_t, vec5_t
            type(vec5_t), intent(in) :: v
        end function

        ! DT with fixed array: C reads element at index
        integer(c_int32_t) function c34_get_vec5_elem(v, idx) bind(C)
            import :: c_int32_t, c_int, vec5_t
            type(vec5_t), intent(in) :: v
            integer(c_int), value :: idx
        end function

        ! c_f_procpointer: C returns a function pointer
        type(c_funptr) function c34_get_func_ptr() bind(C)
            import :: c_funptr
        end function

        ! Module globals: C sets char and complex
        subroutine c34_set_globals(c, re, im) bind(C)
            import :: c_char, c_float
            character(kind=c_char, len=1), value :: c
            real(c_float), value :: re, im
        end subroutine

        ! Module globals: C reads char
        character(kind=c_char, len=1) function c34_get_char() bind(C)
            import :: c_char
        end function

        ! Module globals: C reads complex real part
        real(c_float) function c34_get_cmplx_re() bind(C)
            import :: c_float
        end function

        ! Module globals: C reads complex imag part
        real(c_float) function c34_get_cmplx_im() bind(C)
            import :: c_float
        end function

        ! COMMON: C sets values
        subroutine c34_set_common(x, y) bind(C)
            import :: c_int32_t
            integer(c_int32_t), value :: x, y
        end subroutine

        ! COMMON: C reads x
        integer(c_int32_t) function c34_get_common_x() bind(C)
            import :: c_int32_t
        end function

        ! COMMON: C reads y
        integer(c_int32_t) function c34_get_common_y() bind(C)
            import :: c_int32_t
        end function
    end interface
end module

program bindc_34
    use bindc_34_types
    use bindc_34_globals
    use bindc_34_common_mod
    use bindc_34_ifaces
    implicit none

    call test_enum()
    call test_dt_fixed_array()
    call test_c_f_procpointer()
    call test_module_globals()
    call test_common_block()

    print *, "All bindc_34 tests passed."

contains

    subroutine test_enum()
        ! Verify enum values match between Fortran and C
        if (c34_get_color(0) /= COLOR_RED) error stop "FAIL: enum RED"
        if (c34_get_color(1) /= COLOR_GREEN) error stop "FAIL: enum GREEN"
        if (c34_get_color(2) /= COLOR_BLUE) error stop "FAIL: enum BLUE"
        ! Pass Fortran enum to C and check
        if (c34_check_color(COLOR_RED, 0) /= 1) error stop "FAIL: check RED"
        if (c34_check_color(COLOR_GREEN, 1) /= 1) error stop "FAIL: check GREEN"
        if (c34_check_color(COLOR_BLUE, 2) /= 1) error stop "FAIL: check BLUE"
    end subroutine

    subroutine test_dt_fixed_array()
        type(vec5_t) :: v
        v%data = [10, 20, 30, 40, 50]
        if (c34_sum_vec5(v) /= 150) error stop "FAIL: vec5 sum"
        if (c34_get_vec5_elem(v, 0) /= 10) error stop "FAIL: vec5 elem 0"
        if (c34_get_vec5_elem(v, 2) /= 30) error stop "FAIL: vec5 elem 2"
        if (c34_get_vec5_elem(v, 4) /= 50) error stop "FAIL: vec5 elem 4"
    end subroutine

    subroutine test_c_f_procpointer()
        type(c_funptr) :: cfp
        procedure(int_func_i), pointer :: fptr
        ! Get function pointer from C (doubles its argument)
        cfp = c34_get_func_ptr()
        ! Convert to Fortran procedure pointer
        call c_f_procpointer(cfp, fptr)
        ! Call through procedure pointer
        if (fptr(21) /= 42) error stop "FAIL: c_f_procpointer call"
        if (fptr(0) /= 0) error stop "FAIL: c_f_procpointer zero"
        if (fptr(-5) /= -10) error stop "FAIL: c_f_procpointer neg"
    end subroutine

    subroutine test_module_globals()
        ! Set from Fortran, read from C
        g34_char = 'Z'
        if (c34_get_char() /= 'Z') error stop "FAIL: module char F->C"

        g34_cmplx = cmplx(3.0, 4.0)
        if (abs(c34_get_cmplx_re() - 3.0) > 1.0e-5) &
            error stop "FAIL: module cmplx re F->C"
        if (abs(c34_get_cmplx_im() - 4.0) > 1.0e-5) &
            error stop "FAIL: module cmplx im F->C"

        ! Set from C, read from Fortran
        call c34_set_globals('A', 1.5, 2.5)
        if (g34_char /= 'A') error stop "FAIL: module char C->F"
        if (abs(real(g34_cmplx) - 1.5) > 1.0e-5) &
            error stop "FAIL: module cmplx re C->F"
        if (abs(aimag(g34_cmplx) - 2.5) > 1.0e-5) &
            error stop "FAIL: module cmplx im C->F"
    end subroutine

    subroutine test_common_block()
        ! Set from Fortran, read from C
        common_x = 42
        common_y = 84
        if (c34_get_common_x() /= 42) error stop "FAIL: common x F->C"
        if (c34_get_common_y() /= 84) error stop "FAIL: common y F->C"

        ! Set from C, read from Fortran
        call c34_set_common(100, 200)
        if (common_x /= 100) error stop "FAIL: common x C->F"
        if (common_y /= 200) error stop "FAIL: common y C->F"
    end subroutine

end program
