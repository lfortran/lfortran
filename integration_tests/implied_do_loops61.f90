! Implied-do initializers whose values are real(16), or are given by an
! intrinsic with a kind argument, are evaluated in that kind
module implied_do_loops61_m
implicit none
type :: tq
    real(16) :: x
end type
type :: tl
    logical :: b
end type
type :: tz
    complex :: z
end type
integer :: k
type(tq) :: a(3) = [(tq(k/4.0_16), k=1,3)]
type(tq) :: an(2) = [(tq(-(k/3.0_16)), k=1,2)]
type(tl) :: b(3) = [(tl(real(k,16) > 1.5_16), k=1,3)]
type(tl) :: bo(3) = [(tl(k > 2 .or. k == 1), k=1,3)]
type(tz) :: z(2) = [(tz(cmplx(k, 1) * (0, 1)), k=1,2)]
logical :: c(3) = [(real(k,16) > 1.5_16, k=1,3)]
real(8) :: d(2) = [(real(k,8) / 3, k=1,2)]
end module

program implied_do_loops61
use implied_do_loops61_m
implicit none
if (abs(a(1)%x - 0.25_16) > 1e-30_16) error stop
if (abs(a(2)%x - 0.5_16) > 1e-30_16) error stop
if (abs(a(3)%x - 0.75_16) > 1e-30_16) error stop
if (abs(an(1)%x + 1/3.0_16) > 1e-30_16) error stop
if (abs(an(2)%x + 2/3.0_16) > 1e-30_16) error stop
if (any(b%b .neqv. [.false., .true., .true.])) error stop
if (any(bo%b .neqv. [.true., .false., .true.])) error stop
if (any(c .neqv. [.false., .true., .true.])) error stop
if (z(1)%z /= (-1, 1) .or. z(2)%z /= (-1, 2)) error stop
if (abs(d(1) - 1/3.0_8) > 1e-15_8 .or. abs(d(2) - 2/3.0_8) > 1e-15_8) error stop
print *, "ok"
end program
