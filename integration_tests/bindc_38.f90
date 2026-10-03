! Test: VOLATILE, ASYNCHRONOUS, PURE BIND(C)
!
! Covers:
!   - VOLATILE attribute on BIND(C) module variable
!   - ASYNCHRONOUS attribute on BIND(C) dummy arguments
!   - PURE function with BIND(C)
!   - Passing a BIND(C) procedure as actual argument via C_FUNLOC
module bindc_38_globals
    use iso_c_binding
    implicit none
    integer(c_int), volatile, bind(C, name="g38_volatile_int") :: g38_volatile_int = 0
    real(c_double), volatile, bind(C, name="g38_volatile_dbl") :: g38_volatile_dbl = 0.0d0
end module

module bindc_38_procs
    use iso_c_binding
    implicit none
contains
    pure integer(c_int) function add_pure(a, b) bind(c, name="f38_add_pure")
        integer(c_int), intent(in), value :: a, b
        add_pure = a + b
    end function
end module

module bindc_38_ifaces
    use iso_c_binding
    implicit none

    abstract interface
        integer(c_int) function int_binop_i(a, b) bind(C)
            import :: c_int
            integer(c_int), value :: a, b
        end function
    end interface

    interface
        ! C modifies the volatile variables
        subroutine c38_set_volatile_int(val) bind(c)
            import :: c_int
            integer(c_int), value :: val
        end subroutine

        subroutine c38_set_volatile_dbl(val) bind(c)
            import :: c_double
            real(c_double), value :: val
        end subroutine

        ! C function that takes ASYNCHRONOUS arrays (raw pointer in C)
        subroutine c38_fill_async(arr, n) bind(c)
            import :: c_int
            integer(c_int), intent(inout), asynchronous :: arr(*)
            integer(c_int), value :: n
        end subroutine

        ! C function that invokes a Fortran function pointer
        integer(c_int) function c38_invoke_binop(fp, a, b) bind(c)
            import :: c_int, c_funptr
            type(c_funptr), value :: fp
            integer(c_int), value :: a, b
        end function
    end interface
end module

program bindc_38
    use iso_c_binding
    use bindc_38_globals
    use bindc_38_procs
    use bindc_38_ifaces
    implicit none

    call test_volatile()
    call test_asynchronous()
    call test_pure_bindc()
    call test_proc_as_argument()

    print *, "All bindc_38 tests passed."

contains

    subroutine test_volatile()
        ! Set from Fortran, verify it's stored
        g38_volatile_int = 10
        if (g38_volatile_int /= 10) error stop "FAIL: volatile int F"

        ! Set from C, read from Fortran
        call c38_set_volatile_int(42)
        if (g38_volatile_int /= 42) error stop "FAIL: volatile int C->F"

        g38_volatile_dbl = 1.5d0
        if (abs(g38_volatile_dbl - 1.5d0) > 1.0d-10) &
            error stop "FAIL: volatile dbl F"

        call c38_set_volatile_dbl(3.14d0)
        if (abs(g38_volatile_dbl - 3.14d0) > 1.0d-10) &
            error stop "FAIL: volatile dbl C->F"
    end subroutine

    subroutine test_asynchronous()
        integer(c_int) :: arr(5)
        arr = [1, 2, 3, 4, 5]
        ! C doubles each element
        call c38_fill_async(arr, 5_c_int)
        if (arr(1) /= 2) error stop "FAIL: async arr(1)"
        if (arr(2) /= 4) error stop "FAIL: async arr(2)"
        if (arr(3) /= 6) error stop "FAIL: async arr(3)"
        if (arr(4) /= 8) error stop "FAIL: async arr(4)"
        if (arr(5) /= 10) error stop "FAIL: async arr(5)"
    end subroutine

    subroutine test_pure_bindc()
        integer(c_int) :: r
        r = add_pure(10_c_int, 20_c_int)
        if (r /= 30) error stop "FAIL: pure bindc add"
        r = add_pure(-5_c_int, 5_c_int)
        if (r /= 0) error stop "FAIL: pure bindc zero"
    end subroutine

    subroutine test_proc_as_argument()
        type(c_funptr) :: fp
        integer(c_int) :: r
        ! Get a C function pointer to our BIND(C) function
        fp = c_funloc(add_pure)
        ! Pass it to C, which invokes it
        r = c38_invoke_binop(fp, 7_c_int, 8_c_int)
        if (r /= 15) error stop "FAIL: proc as argument"
    end subroutine

end program
