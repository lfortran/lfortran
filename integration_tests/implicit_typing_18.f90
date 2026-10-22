! A variable first named in a SAVE statement takes its type from the
! scope's IMPLICIT statements, not from the default implicit typing rule.
module implicit_typing_18_mod
implicit double precision (a-h,o-z)
save d
contains
subroutine bump(r)
double precision, intent(out) :: r
save e
data e /0d0/
if (kind(d) /= kind(1d0)) error stop 11
if (kind(e) /= kind(1d0)) error stop 12
e = e + 1d0/3d0
d = e
r = e
end subroutine
end module

subroutine accumulate(n, total, count, flag)
implicit double precision (a-h,o-w)
implicit integer (x)
implicit logical (y)
integer, intent(in) :: n
double precision, intent(out) :: total
integer, intent(out) :: count
logical, intent(out) :: flag
save d, x, y
data d /0d0/, x /0/, y /.false./
if (kind(d) /= kind(1d0)) error stop 1
d = d + 1d0/3d0
x = x + n
if (2*(x/2) == x) error stop 10
y = .not. y
total = d
count = x
flag = y
end subroutine

subroutine counter(k_out)
integer, intent(out) :: k_out
save k
data k /0/
k = k + 1
k_out = k
end subroutine

program implicit_typing_18
use implicit_typing_18_mod, only: bump, d
implicit none
double precision :: total, r
integer :: count, k
logical :: flag
call accumulate(3, total, count, flag)
if (abs(total - 1d0/3d0) > 1d-15) error stop 2
if (count /= 3) error stop 3
if (.not. flag) error stop 4
call accumulate(4, total, count, flag)
if (abs(total - 2d0/3d0) > 1d-15) error stop 5
if (count /= 7) error stop 6
if (flag) error stop 8
call counter(k)
call counter(k)
if (k /= 2) error stop 9
call bump(r)
call bump(r)
if (abs(r - 2d0/3d0) > 1d-15) error stop 13
if (abs(d - 2d0/3d0) > 1d-15) error stop 14
print *, total, count, flag, k, r, d
end program
