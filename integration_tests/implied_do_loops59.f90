! An implied-do array constructor with a negative step initializes a module
! array and a named constant with its values in the order of the loop.
module implied_do_loops59_m
implicit none
type :: t
    integer :: i = 0
end type
integer :: k, k2
integer :: ma(4) = [(10*k, k=4,1,-1)]
integer :: mb(4) = [((10*k + k2, k2=2,1,-1), k=1,2)]
real :: mr(3) = [(real(k), k=5,1,-2)]
type(t) :: mt(4) = [(t(10*k), k=4,1,-1)]
integer, parameter :: mp(3) = [(k, k=9,1,-4)]
end module implied_do_loops59_m

program implied_do_loops59
use implied_do_loops59_m
implicit none
if (any(ma /= [40, 30, 20, 10])) error stop 1
if (any(mb /= [12, 11, 22, 21])) error stop 2
if (any(abs(mr - [5.0, 3.0, 1.0]) > 1e-6)) error stop 3
if (any(mt%i /= [40, 30, 20, 10])) error stop 4
if (any(mp /= [9, 5, 1])) error stop 5
print *, "ok"
end program implied_do_loops59
