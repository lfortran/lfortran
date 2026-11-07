! Implied-do initializers that index a named constant array with lower
! bounds other than 1, of rank 2, or of character type
module implied_do_loops62_m
implicit none
type :: t
    integer :: i
    real :: x = 0
end type
integer :: k
integer, parameter :: p(0:2) = [10, 20, 30]
real, parameter :: r(-1:1) = [1.5, 2.5, 3.5]
logical, parameter :: l(2:4) = [.true., .false., .true.]
type(t) :: a(2) = [(t(p(k)), k=1,2)]
type(t) :: ar(3) = [(t(k, r(k-2)), k=1,3)]
integer :: b(2) = [(p(k), k=1,2)]
real :: c(2) = [(r(k), k=0,1)]
logical :: d(3) = [(l(k), k=4,2,-1)]
integer, parameter :: m(2,0:1) = reshape([1, 2, 3, 4], [2, 2])
integer :: e(2) = [(m(k,1), k=1,2)]
type(t) :: et(2) = [(t(m(2,k)), k=0,1)]
character(len=2), parameter :: s2(3) = ['ab', 'cd', 'ef']
character(len=2) :: f(3) = [(s2(k), k=3,1,-1)]
end module

program implied_do_loops62
use implied_do_loops62_m
implicit none
if (any(a%i /= [20, 30])) error stop
if (any(ar%i /= [1, 2, 3])) error stop
if (any(ar%x /= [1.5, 2.5, 3.5])) error stop
if (any(b /= [20, 30])) error stop
if (any(c /= [2.5, 3.5])) error stop
if (any(d .neqv. [.true., .false., .true.])) error stop
if (any(e /= [3, 4])) error stop
if (any(et%i /= [2, 4])) error stop
if (any(f /= ['ef', 'cd', 'ab'])) error stop
print *, "ok"
end program
