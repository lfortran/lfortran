! Implied-do initializers with logical and complex operators are evaluated
! at compile time to the values of the operators, not of their operands.
module implied_do_loops60_m
implicit none
type :: t
    logical :: b = .false.
end type
type :: tz
    complex :: z = (0.0, 0.0)
end type
integer :: k
logical :: l1(3) = [(.not. (k > 1), k=1,3)]
logical :: l2(3) = [(k > 1 .and. k < 3, k=1,3)]
logical :: l3(3) = [(k < 2 .or. k > 2, k=1,3)]
logical :: l4(3) = [(k > 1 .eqv. k < 3, k=1,3)]
logical :: l5(3) = [(k > 1 .neqv. k < 3, k=1,3)]
complex :: c1(2) = [(cmplx(k, 0) * (0.0, 1.0), k=1,2)]
complex :: c2(2) = [(-cmplx(k, 1), k=1,2)]
logical :: c3(2) = [(cmplx(k, 0) == (1.0, 0.0), k=1,2)]
type(t) :: d1(3) = [(t(.not. (k > 1)), k=1,3)]
type(tz) :: d2(2) = [(tz(cmplx(k, 0) - (0.0, 1.0)), k=1,2)]
end module implied_do_loops60_m

program implied_do_loops60
use implied_do_loops60_m
implicit none
integer :: j
logical, parameter :: p1(3) = [(.not. (j > 1), j=1,3)]
type(t), parameter :: p2(3) = [(t(j == 2 .or. j == 3), j=1,3)]

if (any(l1 .neqv. [.true., .false., .false.])) error stop 1
if (any(l2 .neqv. [.false., .true., .false.])) error stop 2
if (any(l3 .neqv. [.true., .false., .true.])) error stop 3
if (any(l4 .neqv. [.false., .true., .false.])) error stop 4
if (any(l5 .neqv. [.true., .false., .true.])) error stop 5
if (any(c1 /= [(0.0, 1.0), (0.0, 2.0)])) error stop 6
if (any(c2 /= [(-1.0, -1.0), (-2.0, -1.0)])) error stop 7
if (any(c3 .neqv. [.true., .false.])) error stop 8
if (any(d1%b .neqv. [.true., .false., .false.])) error stop 9
if (any(d2%z /= [(1.0, -1.0), (2.0, -1.0)])) error stop 10
if (any(p1 .neqv. [.true., .false., .false.])) error stop 11
if (any(p2%b .neqv. [.false., .true., .true.])) error stop 12
print *, "ok"
end program implied_do_loops60
