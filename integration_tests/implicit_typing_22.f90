! An internal procedure of a module procedure starts from the implicit
! typing rules of its host procedure, not from those of the module
! (F2018 8.7 p4), with or without an IMPLICIT statement of its own.
module implicit_typing_22_mod
implicit double precision (a-h)
contains
subroutine sub_host(ka, kb, ki)
    implicit real (a-h)
    integer, intent(out) :: ka, kb, ki
    c = 1.5
    if (kind(c) /= 4) error stop 1
    call own_implicit(ka, ki)
    call no_implicit(kb)
contains
    subroutine own_implicit(ka, ki)
        implicit integer (i-n)
        integer, intent(out) :: ka, ki
        a = 1
        i = 2
        ka = kind(a)
        ki = kind(i)
    end subroutine

    subroutine no_implicit(kb)
        integer, intent(out) :: kb
        b = 1
        kb = kind(b)
    end subroutine
end subroutine

real function fun_host()
    implicit real (a-h)
    c = 2.5
    if (kind(c) /= 4) error stop 2
    fun_host = inner() + c
contains
    real function inner()
        implicit integer (i-n)
        b = 1.5
        if (kind(b) /= 4) error stop 3
        inner = b
    end function
end function
end module

program implicit_typing_22
use implicit_typing_22_mod
integer :: ka, kb, ki
call sub_host(ka, kb, ki)
print *, ka, kb, ki
if (ka /= 4) error stop 4
if (kb /= 4) error stop 5
if (ki /= 4) error stop 6
print *, fun_host()
if (abs(fun_host() - 4.0) > 1e-6) error stop 7
end program
