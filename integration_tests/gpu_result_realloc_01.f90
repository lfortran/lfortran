! An allocatable result allocated with the shape of the value assigned to it
! right after is not reallocated by that assignment, so the device can give
! it one buffer even when reallocation on assignment is on.
module gpu_result_realloc_01_m
implicit none
type :: data_t
    real, allocatable :: v(:)
end type
contains

pure function column(a, i) result(v)
    real, intent(in) :: a(:, :)
    integer, intent(in) :: i
    real, allocatable :: v(:)
    allocate(v(size(a, 1)))
    v = 2 * a(:, i) + 1
end function

pure function head(a, n) result(v)
    real, intent(in) :: a(:)
    integer, intent(in) :: n
    real, allocatable :: v(:)
    allocate(v(n))
    v = a(1:n)
end function

pure function negated(a) result(v)
    real, intent(in) :: a(:)
    real, allocatable :: v(:)
    allocate(v(size(a)))
    v = -a
end function

pure function component(d) result(r)
    type(data_t), intent(in) :: d
    real, allocatable :: r(:)
    allocate(r(size(d%v)))
    r = d%v
end function

pure function fixed(c) result(v)
    real, intent(in) :: c(3)
    real, allocatable :: v(:)
    allocate(v(3))
    v = c
end function

end module

program gpu_result_realloc_01
use gpu_result_realloc_01_m
implicit none
real :: a(3, 4), b(5), c(3)
real :: s1(4), s2(4), s3(4), s4(2), s5(4)
type(data_t) :: d(2)
integer :: i

do i = 1, 4
    a(:, i) = real(i)
end do
b = [1.0, 2.0, 3.0, 4.0, 5.0]
c = [1.0, 2.0, 4.0]
d(1)%v = [1.0, 2.0]
d(2)%v = [3.0, 4.0, 5.0]

do concurrent (i = 1:4)
    associate(y => column(a, i))
        s1(i) = y(1) + y(3)
    end associate
end do
do concurrent (i = 1:4)
    associate(y => head(b, i))
        s2(i) = y(i)
    end associate
end do
do concurrent (i = 1:4)
    associate(y => negated(b))
        s3(i) = y(i)
    end associate
end do
do concurrent (i = 1:2)
    associate(y => component(d(i)))
        s4(i) = y(1)
    end associate
end do
do concurrent (i = 1:4)
    associate(y => fixed(c))
        s5(i) = y(3) * i
    end associate
end do

print *, s1, s2, s3, s4, s5
if (any(abs(s1 - [6.0, 10.0, 14.0, 18.0]) > 1.0e-5)) error stop
if (any(abs(s2 - [1.0, 2.0, 3.0, 4.0]) > 1.0e-5)) error stop
if (any(abs(s3 - [-1.0, -2.0, -3.0, -4.0]) > 1.0e-5)) error stop
if (any(abs(s4 - [1.0, 3.0]) > 1.0e-5)) error stop
if (any(abs(s5 - [4.0, 8.0, 12.0, 16.0]) > 1.0e-5)) error stop
end program
