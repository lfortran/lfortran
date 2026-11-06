! An internal procedure uses an assumed-size character dummy of its host,
! of assumed or constant length, with a lower bound other than 1, through
! another internal procedure and as an actual argument
module host_assoc_14_m
implicit none
contains
subroutine s1(c, k)
character(len=*) :: c(*)
integer :: k
call inner()
contains
subroutine inner()
if (len(c(k)) /= 2) error stop 1
if (c(k) /= 'ab') error stop 2
c(k) = 'xy'
end subroutine
end subroutine
subroutine s2(c, k)
character(len=2) :: c(*)
integer :: k
call inner()
contains
subroutine inner()
if (c(k + 1) /= 'xy') error stop 3
c(k) = 'uv'
end subroutine
end subroutine
subroutine s3(c, n)
integer :: n
character(len=*) :: c(0:n, *)
call inner()
contains
subroutine inner()
if (c(1, 1) /= 'ab') error stop 4
c(0, 2) = c(0, 2)(2:2) // 'q'
end subroutine
end subroutine
subroutine s4(c, k)
character(len=*) :: c(*)
integer :: k
call middle()
contains
subroutine middle()
call inner()
end subroutine
subroutine inner()
call set(c, k)
if (c(k) /= 'zz') error stop 8
end subroutine
end subroutine
subroutine set(c, k)
character(len=*) :: c(*)
integer :: k
c(k) = repeat('z', len(c))
end subroutine
end module

program host_assoc_14
use host_assoc_14_m
implicit none
character(len=2) :: c(3), d(2, 3)
c = 'ab'
call s1(c, 2)
if (c(1) /= 'ab' .or. c(2) /= 'xy' .or. c(3) /= 'ab') error stop 5
call s2(c, 1)
if (c(1) /= 'uv' .or. c(2) /= 'xy' .or. c(3) /= 'ab') error stop 6
d = 'ab'
call s3(d, 1)
if (d(1, 2) /= 'bq' .or. d(2, 2) /= 'ab' .or. d(1, 3) /= 'ab') error stop 7
call s4(c, 3)
if (c(1) /= 'uv' .or. c(2) /= 'xy' .or. c(3) /= 'zz') error stop 9
print *, c, d
end program
