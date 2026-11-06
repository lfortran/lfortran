module associate_65_ma
implicit none
type :: t
    integer :: v = 1
end type
end module

module associate_65_mb
implicit none
type :: t
    real :: w = 2.5
    integer :: k = 7
end type
type :: boxb
    type(t) :: p
end type
end module

program associate_65
! The program's `t` is associate_65_ma's; `b` has associate_65_mb's `t`,
! which is not accessible here under its own name.
use associate_65_ma, only: t
use associate_65_mb, only: boxb
implicit none
type(t) :: a0
type(boxb) :: yb
associate (b => yb%p)
    print *, b%w, b%k, a0%v
    if (abs(b%w - 2.5) > 1e-6) error stop 1
    if (b%k /= 7) error stop 2
    b%k = 9
end associate
if (yb%p%k /= 9) error stop 3
if (a0%v /= 1) error stop 4
end program
