! The result of an untyped external procedure pointer declared in an internal
! procedure of a main program has the implicit type of the program, which
! the internal procedure inherits (#16414)
program procedure_pointer_50
implicit double precision (a-h, o-z)
call s()
print *, "ok"
contains
subroutine s()
    pointer :: fp
    external :: fp
    fp => half
    y = fp(3.0d0)
    if (kind(y) /= 8) error stop 1
    if (abs(y - 1.5d0) > 1d-12) error stop 2
end subroutine
double precision function half(x)
    double precision :: x
    half = x / 2
end function
end program
