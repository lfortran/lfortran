program allocate_01
implicit none
integer, allocatable :: a(:), e(:)
real, allocatable :: b(:, :), f(:, :)
complex, allocatable :: c(:, :, :), g(:, :, :)
integer :: d(5) = [1, 2, 3, 4, 5]
complex :: r
integer :: n, ierr
integer :: i, j, k
n = 10

allocate(e, mold=d)
if (size(e) /= 5) error stop

allocate(a, mold=e)
if (size(a) /= 5) error stop
deallocate(a)

allocate(b(2, 3))
allocate(f, mold=b)
if (size(f, 1) /= 2 .or. size(f, 2) /= 3) error stop
deallocate(b)

allocate(c(2, 3, 4))
allocate(g, mold=c)
if (size(g, 1) /= 2 .or. size(g, 2) /= 3 .or. size(g, 3) /= 4) error stop
deallocate(c)

allocate(a(5:n + 5))
allocate(b(n:2*n, n:3*n), stat=ierr)
if( size(a) /= n + 1 ) error stop
if( size(b) /= (n + 1)*(2*n + 1) ) error stop
do i = lbound(a, 1), ubound(a, 1)
    a(i) = i
end do
do i = lbound(b, 1), ubound(b, 1)
    do j = lbound(b, 2), ubound(b, 2)
        b(i, j) = i + j
    end do
end do

call sum(a, b, c)

if (lbound(c, 1) /= 5 .or. ubound(c, 1) /= n + 5) error stop
if (lbound(c, 2) /= n .or. ubound(c, 2) /= 2 * n) error stop
if (lbound(c, 3) /= n .or. ubound(c, 3) /= 3 * n) error stop

do i = lbound(a, 1), ubound(a, 1)
    if (a(i) /= i) error stop
end do
do i = lbound(b, 1), ubound(b, 1)
    do j = lbound(b, 2), ubound(b, 2)
        if (b(i, j) /= i + j) error stop
    end do
end do
do i = lbound(c, 1), ubound(c, 1)
    do j = lbound(c, 2), ubound(c, 2)
        do k = lbound(c, 3), ubound(c, 3)
            if (c(i, j, k) /= i + j + k) error stop
        end do
    end do
end do

r = reduce_sum(c)
if (r /= (114345.0, 0.0)) error stop

contains

subroutine sum(a, b, c)
implicit none

integer, allocatable, intent(in) :: a(:)
real, allocatable, intent(in) :: b(:, :)
complex, allocatable, intent(out) :: c(:, :, :)
integer :: i, j, k

complex, allocatable :: c_copy(:, :, :)

allocate(c_copy(lbound(a, 1):ubound(a, 1), lbound(b, 1):ubound(b, 1), lbound(b, 2):ubound(b, 2)))
allocate(c(lbound(a, 1):ubound(a, 1), lbound(b, 1):ubound(b, 1), lbound(b, 2):ubound(b, 2)))

do i = lbound(a, 1), ubound(a, 1)
    do j = lbound(b, 1), ubound(b, 1)
        do k = lbound(b, 2), ubound(b, 2)
            c_copy(i, j, k) = a(i) + b(j, k)
            c(i, j, k) = a(i) + b(j, k)
        end do
    end do
end do

deallocate(c_copy)

end subroutine sum

complex function reduce_sum(c) result(r)
implicit none

complex, intent(in) :: c(:, :, :)
integer :: i, j, k

r = 0

do i = lbound(c, 1), ubound(c, 1)
    do j = lbound(c, 2), ubound(c, 2)
        do k = lbound(c, 3), ubound(c, 3)
            r = r + c(i, j, k)
        end do
    end do
end do

end function reduce_sum

end
