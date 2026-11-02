! The IMPLICIT statement of a main program with internal procedures applies
! to the program's body and to its internal procedures without an IMPLICIT
! statement (F2018 8.7 p4); an internal procedure's own IMPLICIT statement
! applies in it, and the letters it does not name keep the program's mapping.
program implicit_typing_21
implicit double precision (a-h, o-z)
r = 1d0 / 3d0
if (kind(r) /= 8) error stop 1
if (abs(r - 1d0 / 3d0) > 1d-15) error stop 2
call set_third(t)
if (abs(t - 1d0 / 3d0) > 1d-15) error stop 3
call internal_local(w)
if (abs(w - 1d0 / 3d0) > 1d-15) error stop 4
call own_implicit(k)
if (k /= 4) error stop 5
call own_implicit_host_letter(v)
if (abs(v - 1d0 / 3d0) > 1d-15) error stop 7
print *, r, t, w, k
contains
subroutine set_third(z)
    double precision, intent(out) :: z
    z = 1d0 / 3d0
end subroutine

subroutine internal_local(z)
    double precision, intent(out) :: z
    d = 1d0 / 3d0
    if (kind(d) /= 8) error stop 6
    z = d
end subroutine

subroutine own_implicit(n)
    implicit real (d)
    integer, intent(out) :: n
    d = 1
    n = kind(d)
end subroutine

subroutine own_implicit_host_letter(z)
    implicit integer (i-n)
    double precision, intent(out) :: z
    e = 1d0 / 3d0
    if (kind(e) /= 8) error stop 8
    j = 7
    if (kind(j) /= 4) error stop 9
    z = e
end subroutine
end program
