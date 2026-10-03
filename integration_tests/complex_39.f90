program complex_39
implicit none

type :: inner
    complex :: c
end type inner

type(inner) :: d

d%c = (1.0, 2.0)

! assign to a component's real and imaginary part
d%c%re = 5.0
d%c%im = -3.0

! pass a component's part as an actual argument to a dummy with unspecified intent
call double_it(d%c%re)
if (abs(d%c%re - 10.0) > 1e-6) error stop
if (abs(d%c%im + 3.0) > 1e-6) error stop
call double_it(d%c%im)
if (abs(d%c%re - 10.0) > 1e-6) error stop
if (abs(d%c%im + 6.0) > 1e-6) error stop

contains

subroutine double_it(r)
real :: r
    r = r*2
end subroutine double_it

end program complex_39
