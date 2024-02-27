program intrinsics_51
implicit none

real :: x = 1.0
real, allocatable :: a(:, :), dr(:), er(:)
real :: b(2, 2, 2), cr(5)
integer :: i, j, k
allocate(a(3, 2), dr(10), er(13))

do i = lbound(b, 1), ubound(b, 1)
    do j = lbound(b, 2), ubound(b, 2)
        do k = lbound(b, 3), ubound(b, 3)
            b(i, j, k) = 4*i - 5*j - 6*k
        end do
    end do
end do

do i = lbound(a, 1), ubound(a, 1)
    do j = lbound(a, 2), ubound(a, 2)
        a(i, j) = 4*i + 5*j
    end do
end do

print *, [1., 2., 3., 4., 5.]
print *, "basic", sin([1., 2., 3., 4., 5.])


print *, [[1., 2., 3.], [4., 5.]]
cr = sin([[1., 2., 3.], [4., 5.]])
print *, "cr: ", cr
do i = lbound(cr, 1), ubound(cr, 1)
    if( abs(cr(i) - sin(real(i))) > 1e-6 ) error stop
end do

print *, [[1., 2., 3.], a, 5.]
dr = sin([[1., 2., 3.], a, 5.])
print *, "dr.1: ", dr
do i = 1, 3
    if( abs(dr(i) - sin(real(i))) > 1e-6 ) error stop
end do
k = 4
do j = lbound(a, 2), ubound(a, 2)
    do i = lbound(a, 1), ubound(a, 1)
    if( abs(dr(k) - sin(real(4*i + 5*j))) > 1e-6 ) error stop
    k = k + 1
    end do
end do
if( abs(dr(k) - sin(5.)) > 1e-6 ) error stop

print *, [[1., 2., b(:, 1, :)], a, 5.]
er = sin([[1., 2., b(:, 1, :)], a, 5.])
print *, "er: ", er
do i = 1, 2
    if( abs(er(i) - sin(real(i))) > 1e-6 ) error stop
end do
k = 3
do j = lbound(b, 3), ubound(b, 3)
    do i = lbound(b, 1), ubound(b, 1)
    if( abs(er(k) - sin(real(4*i - 5 - 6*j))) > 1e-6 ) error stop
    k = k + 1
    end do
end do
do j = lbound(a, 2), ubound(a, 2)
    do i = lbound(a, 1), ubound(a, 1)
    if( abs(er(k) - sin(real(4*i + 5*j))) > 1e-6 ) error stop
    k = k + 1
    end do
end do
if( abs(er(k) - sin(5.)) > 1e-6 ) error stop

print *, [[1., 2., x], a, 5.]
dr = sin([[1., 2., x], a, 5.])
print *, "dr.2: ", dr
do i = 1, 2
    if( abs(dr(i) - sin(real(i))) > 1e-6 ) error stop
end do
if( abs(dr(3) - sin(x)) > 1e-6 ) error stop
k = 4
do j = lbound(a, 2), ubound(a, 2)
    do i = lbound(a, 1), ubound(a, 1)
    if( abs(dr(k) - sin(real(4*i + 5*j))) > 1e-6 ) error stop
    k = k + 1
    end do
end do
if( abs(dr(k) - sin(5.)) > 1e-6 ) error stop

end program
