module types_25_mod
implicit none
type(integer) :: mi(3) = [7, 8, 9]
type :: t
    type(integer) :: c(2) = [1, 2]
    type(real(8)) :: d = 2 + 0.5d0
end type
contains
subroutine counter(n)
    integer, intent(out) :: n
    ! An initialized local has the implicit save attribute
    type(integer) :: k = 5
    n = k
    k = k + 1
end subroutine
end module

program types_25
use types_25_mod
implicit none
type(integer) :: ti(3) = [1, 2, 3]
type(real(8)) :: r(2) = [1.5d0, 2.5d0]
type(logical) :: l(2) = [.true., .false.]
type(character(len=3)) :: c(2) = ['abc', 'def']
type(complex) :: z(2) = [(1.0, 2.0), (3.0, 4.0)]
type(integer), parameter :: ip(3) = [4, 5, 6]
type(integer) :: tb(2) = 7
type(integer) :: sc = 5
type(integer) :: se = 2 + 3
type(t) :: x
integer :: n

print *, ti, r, l, c, z, ip, tb, sc, se
if (any(ti /= [1, 2, 3])) error stop
if (any(r /= [1.5d0, 2.5d0])) error stop
if (.not. l(1) .or. l(2)) error stop
if (c(1) /= 'abc' .or. c(2) /= 'def') error stop
if (len(c) /= 3) error stop
if (z(1) /= (1.0, 2.0) .or. z(2) /= (3.0, 4.0)) error stop
if (any(ip /= [4, 5, 6])) error stop
if (any(tb /= 7)) error stop
if (sc /= 5 .or. se /= 5) error stop
if (any(mi /= [7, 8, 9])) error stop
if (any(x%c /= [1, 2]) .or. x%d /= 2.5d0) error stop
if (kind(r) /= 8) error stop
call counter(n)
if (n /= 5) error stop
call counter(n)
if (n /= 6) error stop
end program
