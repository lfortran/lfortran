program implicit_interface_110
! External functions whose names LFortran also uses for internal
! (non-Fortran) intrinsics, called through an implicit interface.
implicit none
double precision :: diff, expand, has, pi, stringconcat
double precision :: exp2, expm1, fix, trunc, floordiv, fma, type
if (abs(diff(7.5d0, 2d0) - 5.5d0) > 1d-12) error stop 1
if (abs(expand(7.5d0) - 16d0) > 1d-12) error stop 2
if (abs(has(7.5d0, 2d0) - 9.5d0) > 1d-12) error stop 3
if (abs(pi() - 3d0) > 1d-12) error stop 4
if (abs(stringconcat(7.5d0, 2d0) - 15d0) > 1d-12) error stop 5
if (abs(exp2(7.5d0) - 16d0) > 1d-12) error stop 6
if (abs(expm1(7.5d0) - 6.5d0) > 1d-12) error stop 7
if (abs(fix(7.5d0) - 8.5d0) > 1d-12) error stop 8
if (abs(trunc(7.5d0) - 22.5d0) > 1d-12) error stop 9
if (abs(floordiv(7.5d0, 2d0) - 3.75d0) > 1d-12) error stop 10
if (abs(fma(7.5d0, 2d0, 1d0) - 14d0) > 1d-12) error stop 11
if (abs(type(7.5d0) - 4.5d0) > 1d-12) error stop 12
print *, "ok"
end program
