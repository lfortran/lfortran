program intrinsics_483
! Both translation units instantiate the same `sum`, `sin` and string
! concatenation helpers; they must link together (#14496, #11642, #14179).
implicit none
interface
    subroutine intrinsics_483_sub(r, s, c)
        real, intent(out) :: r, s
        character(:), allocatable, intent(out) :: c
    end subroutine
end interface
real :: r, s, x, y(2)
character(:), allocatable :: c
character(2) :: d
y = 2
x = 1
d = "de"
call intrinsics_483_sub(r, s, c)
c = c // d
print *, r, s, sum(y), sin(x), c
if (abs(r + sum(y) - 6) > 1e-6) error stop
if (abs(s - sin(x)) > 1e-6) error stop
if (c /= "abcde") error stop
end program
