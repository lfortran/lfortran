! INTENT on dummy arguments that appear only in an ENTRY statement (#18406):
! in a type declaration statement and in an INTENT statement
subroutine entry_34_s(n, total)
integer, intent(in) :: n, m
integer, intent(out) :: total
real, intent(out) :: x(n)
real :: y
intent(out) :: y
total = n
return
entry entry_34_e(n, m, x, y)
x = real(m)
y = real(n + m)
end subroutine

program entry_34
implicit none
integer :: total
real :: a(3), b
call entry_34_s(3, total)
if (total /= 3) error stop 1
call entry_34_e(3, 4, a, b)
if (any(abs(a - 4) > 1e-6)) error stop 2
if (abs(b - 7) > 1e-6) error stop 3
print *, total, a, b
end program
