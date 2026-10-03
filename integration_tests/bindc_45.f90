! Test: RECURSIVE attribute with BIND(C) procedures
!
! Features tested:
!   - Self-recursive BIND(C) function (factorial)
!   - Mutual recursion between Fortran and C (fibonacci)
!     Each recursive call alternates: Fortran -> C -> Fortran -> ...

module bindc_45_mod
    use iso_c_binding
    implicit none

    interface
        integer(c_int) function c45_mutual_fib(n) bind(C)
            import :: c_int
            integer(c_int), value :: n
        end function
    end interface

contains
    recursive integer(c_int) function f45_factorial(n) &
            bind(C, name="f45_factorial") result(res)
        integer(c_int), value, intent(in) :: n
        if (n <= 1) then
            res = 1
        else
            res = n * f45_factorial(n - 1)
        end if
    end function

    recursive integer(c_int) function f45_fib(n) &
            bind(C, name="f45_fib") result(res)
        integer(c_int), value, intent(in) :: n
        if (n <= 0) then
            res = 0
        else if (n == 1) then
            res = 1
        else
            res = c45_mutual_fib(n - 1) + c45_mutual_fib(n - 2)
        end if
    end function
end module

program bindc_45
    use iso_c_binding
    use bindc_45_mod
    implicit none

    ! Test 1: Self-recursive BIND(C) (factorial)
    if (f45_factorial(0) /= 1)        error stop "FAIL: fact(0)"
    if (f45_factorial(1) /= 1)        error stop "FAIL: fact(1)"
    if (f45_factorial(5) /= 120)      error stop "FAIL: fact(5)"
    if (f45_factorial(10) /= 3628800) error stop "FAIL: fact(10)"

    ! Test 2: Mutual recursion Fortran<->C (fibonacci)
    if (c45_mutual_fib(0) /= 0)   error stop "FAIL: fib(0)"
    if (c45_mutual_fib(1) /= 1)   error stop "FAIL: fib(1)"
    if (c45_mutual_fib(5) /= 5)   error stop "FAIL: fib(5)"
    if (c45_mutual_fib(10) /= 55) error stop "FAIL: fib(10)"

    print *, "All bindc_45 tests passed."
end program
