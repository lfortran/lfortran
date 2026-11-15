! An allocatable result allocated with one shape and then assigned a value
! that need not have it is reallocated to the shape of the value. The device
! cannot do that with its one result buffer, so the offload declines these
! loops (see the reference outputs); on the host each result takes the shape
! of the value.
module gpu_result_realloc_02_m
implicit none
contains

! The allocation does not read the value's shape.
pure function other_extent(a, n) result(v)
    real, intent(in) :: a(:)
    integer, intent(in) :: n
    real, allocatable :: v(:)
    allocate(v(n))
    v = a
end function

! The extent of the other dimension of `a`.
pure function other_dimension(a, i) result(v)
    real, intent(in) :: a(:, :)
    integer, intent(in) :: i
    real, allocatable :: v(:)
    allocate(v(size(a, 1)))
    v = a(i, :)
end function

! A section that does not start at 1 has a different extent.
pure function shifted(a, n) result(v)
    real, intent(in) :: a(:)
    integer, intent(in) :: n
    real, allocatable :: v(:)
    allocate(v(n))
    v = a(2:n)
end function

! The extent changes between the allocation and the assignment.
pure function changed(a, n) result(v)
    real, intent(in) :: a(:)
    integer, intent(in) :: n
    real, allocatable :: v(:)
    integer :: m
    m = n
    allocate(v(m))
    m = m + 1
    v = a(1:m)
end function

end module

program gpu_result_realloc_02
use gpu_result_realloc_02_m
implicit none
real :: a(3, 4), b(5)
integer :: s1(4), s2(3), s3(4), s4(4)
integer :: i

do i = 1, 4
    a(:, i) = real(i)
end do
b = [1.0, 2.0, 3.0, 4.0, 5.0]

do concurrent (i = 1:4)
    associate(y => other_extent(b, i))
        s1(i) = size(y)
    end associate
end do
do concurrent (i = 1:3)
    associate(y => other_dimension(a, i))
        s2(i) = size(y)
    end associate
end do
do concurrent (i = 1:4)
    associate(y => shifted(b, i + 1))
        s3(i) = size(y)
    end associate
end do
do concurrent (i = 1:4)
    associate(y => changed(b, i))
        s4(i) = size(y)
    end associate
end do

print *, s1, s2, s3, s4
if (any(s1 /= 5)) error stop
if (any(s2 /= 4)) error stop
if (any(s3 /= [1, 2, 3, 4])) error stop
if (any(s4 /= [2, 3, 4, 5])) error stop
end program
