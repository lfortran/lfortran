program intrinsics_333
implicit none

real(4) :: rhobeg = 10000
real(4), allocatable :: x0(:), xl(:), xu(:)
logical, allocatable :: lbx(:)
logical, allocatable :: ubx(:)
real(4), parameter :: EPS = -10000

allocate(x0(10), xl(10), xu(10))
allocate(lbx(size(x0)), ubx(size(x0)))
x0(1:10:2) = 11
x0(2:10:2) = 18

lbx(1:10:2) = .true.
lbx(2:10:2) = .false.
ubx(2:10:2) = .false.
ubx(1:10:2) = .true.

xl(1:10:2) = 1
xl(2:10:2) = 2
xu(1:10:2) = 7
xu(2:10:2) = 10


rhobeg = max(EPS, minval([rhobeg, x0(falseloc(lbx)) - xl(falseloc(lbx)), xu(falseloc(ubx)) - x0(falseloc(ubx))]))
print *, rhobeg
if( rhobeg /= -8.0 ) error stop

contains

function falseloc(x) result(y)
logical :: x(:)
integer :: i, j
integer, allocatable :: y(:)

allocate(y(size(x) - count(x)))
j = 1
do i = 1, size(x)
    if( .not. x(i) ) then
        y(j) = i
        j = j + 1
    end if
end do

end function

end program
