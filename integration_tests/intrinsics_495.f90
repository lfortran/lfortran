program intrinsics_495
! pack, cshift, eoshift, maxval and minval of a character array whose length
! is not a constant: a substring of an assumed-length dummy, and an
! explicit-length dummy (#15132)
implicit none
character(len=3) :: x(3)
x = ['abc', 'def', 'ghi']
call assumed_len(x)
call explicit_len(x, 3)

contains

subroutine assumed_len(a)
character(len=*), intent(in) :: a(:)
character(len=2) :: r(3)

r = pack(a(:)(2:), .true.)
print *, r
if (r(1) /= 'bc' .or. r(2) /= 'ef' .or. r(3) /= 'hi') error stop

r = pack(a(:)(2:), [.false., .true., .true.], ['xy', 'zw', 'uv'])
print *, r
if (r(1) /= 'ef' .or. r(2) /= 'hi' .or. r(3) /= 'uv') error stop

r = cshift(a(:)(2:), 1)
print *, r
if (r(1) /= 'ef' .or. r(2) /= 'hi' .or. r(3) /= 'bc') error stop

r = eoshift(a(:)(2:), -1, 'zz')
print *, r
if (r(1) /= 'zz' .or. r(2) /= 'bc' .or. r(3) /= 'ef') error stop

if (len(pack(a(:)(2:), .true.)) /= 2) error stop
if (size(pack(a(:)(2:), [.true., .false., .true.])) /= 2) error stop
if (maxval(a(:)(2:)) /= 'hi') error stop
if (minval(a(:)(2:)) /= 'bc') error stop
end subroutine

subroutine explicit_len(a, n)
integer, intent(in) :: n
character(len=n), intent(in) :: a(:)
character(len=3) :: r(3)
character(len=1) :: q(3)

r = pack(a, .true.)
print *, r
if (r(1) /= 'abc' .or. r(2) /= 'def' .or. r(3) /= 'ghi') error stop

r = cshift(a, -1)
print *, r
if (r(1) /= 'ghi' .or. r(2) /= 'abc' .or. r(3) /= 'def') error stop

q = eoshift(a(:)(n:), 2, '*')
print *, q
if (q(1) /= 'i' .or. q(2) /= '*' .or. q(3) /= '*') error stop

if (maxval(a) /= 'ghi') error stop
if (len(minval(a(:)(2:))) /= 2) error stop
end subroutine

end program
