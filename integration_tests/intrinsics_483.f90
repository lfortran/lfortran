program intrinsics_483
! Both translation units instantiate the same `sum` and `sin` helpers; they
! must link together (#14496, #11642).
implicit none
interface
    subroutine intrinsics_483_sub(r, s)
        real, intent(out) :: r, s
    end subroutine
end interface
real :: r, s, x, y(2)
y = 2
x = 1
call intrinsics_483_sub(r, s)
print *, r, s, sum(y), sin(x)
if (abs(r + sum(y) - 6) > 1e-6) error stop
if (abs(s - sin(x)) > 1e-6) error stop
end program
