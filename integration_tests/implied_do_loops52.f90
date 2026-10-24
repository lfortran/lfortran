program implied_do_loops52
! An output implied-do loop with literal bounds and values of one derived
! type and of intrinsic types is expanded at compile time with more values
! than the limit for values of different derived types.
implicit none
type :: pa
    integer :: i
end type
integer, parameter :: n = 600
type(pa) :: x(n)
integer :: i, j, k(n), u
character(len=10*n+20) :: line
character(len=10) :: pair

do i = 1, n
    x(i)%i = i
    k(i) = -i
end do

! 1200 values, written to an internal file.
line = ''
write(line, '(*(i5,i5))') (x(i), k(i), i=1,600)
do i = 1, n
    write(pair, '(i5,i5)') i, -i
    if (line(10*i-9:10*i) /= pair) error stop 1
end do
if (len_trim(line(10*n+1:)) /= 0) error stop 2

! 1202 values, with a nested loop over the derived type, written to an
! external file as one record.
open(newunit=u, status='scratch', action='readwrite', form='formatted')
write(u, '(*(i5))') (k(i), (x(j), j=1,600), i=1,2)
rewind(u)
line = ''
read(u, '(a)') line
close(u)
do i = 1, 2
    write(pair, '(i5)') -i
    if (line(5*(n+1)*(i-1)+1:5*(n+1)*(i-1)+5) /= pair(1:5)) error stop 3
    do j = 1, n
        write(pair, '(i5)') j
        if (line(5*(n+1)*(i-1)+5*j+1:5*(n+1)*(i-1)+5*j+5) /= pair(1:5)) error stop 4
    end do
end do
if (len_trim(line(10*(n+1)+1:)) /= 0) error stop 5

print '(a)', line(1:20)
end program
