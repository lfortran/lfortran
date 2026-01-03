program matrix_01_tranpose
implicit none
integer :: i, j, a(3, 4), b(4, 3)
real :: x(5, 5), y(5, 5)
double precision :: d(1,2), e(2,1)
complex :: c(2, 2), f(2, 2)
logical :: l(12, 31), m(31, 12)
a = reshape( [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12], [3, 4] )
b = transpose(a)

do i = lbound(a, 1), ubound(a, 1)
    do j = lbound(a, 2), ubound(a, 2)
        if (a(i, j) /= b(j, i)) error stop
    end do
end do

x = reshape( [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25], [5, 5] )
y = transpose(x)

do i = lbound(x, 1), ubound(x, 1)
    do j = lbound(x, 2), ubound(x, 2)
        if (abs(x(i, j) - y(j, i)) > 1e-6) error stop
    end do
end do

d = reshape( [1, 2], [1, 2] )
e = transpose(d)

do i = lbound(d, 1), ubound(d, 1)
    do j = lbound(d, 2), ubound(d, 2)
        if (abs(d(i, j) - e(j, i)) > 1e-12) error stop
    end do
end do

c = reshape( [ (1, 2), (3, 4), (5, 6), (7, 8) ], [2, 2] )
f = transpose(c)

do i = lbound(c, 1), ubound(c, 1)
    do j = lbound(c, 2), ubound(c, 2)
        if (abs(real(c(i, j)) - real(f(j, i))) > 1e-6) error stop
        if (abs(aimag(c(i, j)) - aimag(f(j, i))) > 1e-6) error stop
    end do
end do

l = .true.
m = transpose(l)

do i = lbound(l, 1), ubound(l, 1)
    do j = lbound(l, 2), ubound(l, 2)
        if (l(i, j) .neqv. m(j, i)) error stop
    end do
end do
end program matrix_01_tranpose
