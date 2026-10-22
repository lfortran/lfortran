program implied_do_loops50
! Output implied-do loops with values of different types and literal
! bounds are expanded at compile time: with derived-type values up to the
! expansion limit, and with more intrinsic values than that.
implicit none
type :: pa
    integer :: i
end type
type :: pb
    real :: r
end type
integer, parameter :: n = 500, m = 600
type(pa) :: x(n)
type(pb) :: y(n)
integer :: i, k(m)
real :: r(m)
character(len=10*m) :: line
character(len=10) :: pair

do i = 1, m
    k(i) = i
    r(i) = i + 0.5
end do
x%i = k(1:n)
y%r = r(1:n)

! 1000 values of two derived types, the most that is expanded.
line = ''
write(line, '(*(i4,f6.1))') (x(i), y(i), i=1,500)
do i = 1, n
    write(pair, '(i4,f6.1)') i, i + 0.5
    if (line(10*i-9:10*i) /= pair) error stop 1
end do
if (len_trim(line(10*n+1:)) /= 0) error stop 2

! 1200 values of intrinsic types are expanded too.
line = ''
write(line, '(*(i4,f6.1))') (k(i), r(i), i=1,600)
do i = 1, m
    write(pair, '(i4,f6.1)') i, i + 0.5
    if (line(10*i-9:10*i) /= pair) error stop 3
end do

print '(a)', line(1:20)
end program
