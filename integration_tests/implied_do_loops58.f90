! An implied-do array constructor of structure constructors initializes a
! module array, a saved array and a named constant of a derived type (#13226).
module implied_do_loops58_m
implicit none
type :: t
    integer :: i = 0
end type
type :: pair
    integer :: a = 1
    real :: x = 2.0
    type(t) :: inner
end type
type :: vec
    integer :: v(2)
    logical :: b
end type
integer :: k, k2
type(t) :: mc(3) = [(t(k), k=1,3)]
type(t) :: mn(6) = [((t(10*k + k2), k2=1,2), k=1,3)]
type(pair) :: mp(2) = [(pair(k, k, t(k + 100)), k=1,2)]
type(vec) :: mv(3) = [(vec([k, -k], k > 1), k=1,3)]
type(t), parameter :: mpar(3) = [(t(k*k), k=1,3)]
end module implied_do_loops58_m

program implied_do_loops58
use implied_do_loops58_m
implicit none
integer :: j
type(t), save :: ps(3) = [(t(j - 1), j=1,3)]
type(t), parameter :: pp(2) = [(t(-j), j=1,2)]

if (any(mc%i /= [1, 2, 3])) error stop 1
if (any(mn%i /= [11, 12, 21, 22, 31, 32])) error stop 2
if (any(mp%a /= [1, 2])) error stop 3
if (any(abs(mp%x - [1.0, 2.0]) > 1e-6)) error stop 4
if (any(mp%inner%i /= [101, 102])) error stop 5
if (any(mv(3)%v /= [3, -3]) .or. any(mv%v(1) /= [1, 2, 3])) error stop 6
if (mv(1)%b .or. .not. mv(2)%b) error stop 7
if (any(mpar%i /= [1, 4, 9])) error stop 8
if (any(ps%i /= [0, 1, 2])) error stop 9
if (any(pp%i /= [-1, -2])) error stop 10

! The initialized arrays stay assignable.
mc(2)%i = 20
if (any(mc%i /= [1, 20, 3])) error stop 11

call check_local()
call check_local()
print *, "ok"

contains

    subroutine check_local()
    integer :: n
    type(t) :: loc(3) = [(t(n + 5), n=1,3)]
    if (any(loc%i /= [6, 7, 8])) error stop 12
    end subroutine check_local

end program implied_do_loops58
