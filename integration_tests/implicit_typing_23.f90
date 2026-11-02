! `implicit none (external)` alone keeps the implicit typing rules
! (F2018 8.7): in the scoping unit itself, in its contained procedures, and
! together with an IMPLICIT statement.
module implicit_typing_23_mod
implicit none (external)
contains
subroutine mod_proc(k, kd)
    implicit double precision (a-h)
    integer, intent(out) :: k, kd
    i = 2
    d = 1
    k = kind(i)
    kd = kind(d)
end subroutine
end module

program implicit_typing_23
use implicit_typing_23_mod
implicit none (external)
implicit double precision (d)
integer :: k1, k2
i = 2
if (kind(i) /= 4) error stop 1
x = 1.5
if (kind(x) /= 4) error stop 2
d = 1
if (kind(d) /= 8) error stop 3
call internal(k1)
if (k1 /= 4) error stop 4
call mod_proc(k1, k2)
if (k1 /= 4) error stop 5
if (k2 /= 8) error stop 6
print *, i, x, d
contains
subroutine internal(k)
    integer, intent(out) :: k
    j = 3
    k = kind(j)
    e = 1
    if (kind(e) /= 4) error stop 7
    d = 1
    if (kind(d) /= 8) error stop 8
end subroutine
end program
