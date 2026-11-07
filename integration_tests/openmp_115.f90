module openmp_115_mod
implicit none
contains
    subroutine add3(a, b, c, s)
    integer, intent(in) :: a, b, c
    integer, intent(inout) :: s
    s = s + a + b + c
    end subroutine
end module

program openmp_115
! OpenMP conditional compilation: a `!$` line ending with `&` continues on
! the next `!$` line, optionally followed by `&`; comment and blank lines may
! appear in between
use openmp_115_mod, only: add3
implicit none
integer :: k, m, s, after
k = 0
!$ k = 1 + &
!$     2
if (k /= 3) error stop

m = 0
!$ m = 10 * &
!$&4
if (m /= 40) error stop

s = 0
!$ call add3(1, &
! a comment line between continued lines

!$   & 2, &
!$ 3, s)
if (s /= 6) error stop

after = 1
!$ after = after + &
!$   & 1
after = after * 10
if (after /= 20) error stop
print *, k, m, s, after
end program
