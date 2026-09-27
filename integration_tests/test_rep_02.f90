#ifdef __LFORTRAN__
#define REP(s, n) _lfortran_rep(s, n)
#else
#define REP(s, n) repeat(s, n)
#endif
program test_rep_02
    ! The string repeat helper must use the counted length of the source
    ! string: Fortran character storage is not NUL-terminated.
    implicit none
    character(len=:), allocatable :: x, y

    ! Shrinking in place leaves the old bytes after the new length
    x = "HelloWorld"
    x = x(1:5)
    y = REP(x, 3)
    if (len(y) /= 15) error stop 1
    if (y /= "HelloHelloHello") error stop 2

    x = "abcdefghij"
    x = x(2:4)
    y = REP(x, 2)
    if (len(y) /= 6) error stop 3
    if (y /= "bcdbcd") error stop 4

    ! An embedded NUL is part of the string
    x = "a" // achar(0) // "b"
    y = REP(x, 2)
    if (len(y) /= 6) error stop 5
    if (y /= x // x) error stop 6

    y = REP(x, 0)
    if (len(y) /= 0) error stop 7

    print *, "ok"
end program
