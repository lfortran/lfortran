program arrays_reshape_40
implicit none

type :: pair
    integer :: i
    real :: r
end type

type(pair), parameter :: v(*,*) = reshape( &
    [pair(1, 2.), pair(3, 4.), pair(5, 6.), pair(7, 8.)], [2, 2])

type(pair), parameter :: w(*) = [pair(10, 20.), pair(30, 40.)]

if (v(1,1)%i /= 1) error stop
if (v(1,1)%r /= 2.) error stop
if (v(2,1)%i /= 3) error stop
if (v(2,1)%r /= 4.) error stop
if (v(1,2)%i /= 5) error stop
if (v(1,2)%r /= 6.) error stop
if (v(2,2)%i /= 7) error stop
if (v(2,2)%r /= 8.) error stop

if (w(1)%i /= 10) error stop
if (w(1)%r /= 20.) error stop
if (w(2)%i /= 30) error stop
if (w(2)%r /= 40.) error stop

print *, "ok"
end program
