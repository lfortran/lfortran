! A variable first named in a SAVE statement takes its type from the
! scope's IMPLICIT statements, not from the default implicit typing rule.
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
implicit none
double precision :: total
integer :: count, k
logical :: flag
call accumulate(3, total, count, flag)
if (abs(total - 1d0/3d0) > 1d-15) error stop 2
if (count /= 3) error stop 3
if (.not. flag) error stop 4
call accumulate(4, total, count, flag)
if (abs(total - 2d0/3d0) > 1d-15) error stop 5
if (count /= 7) error stop 6
if (count/2 /= 3) error stop 7
if (flag) error stop 8
call counter(k)
call counter(k)
if (k /= 2) error stop 9
print *, total, count, flag, k
end program
