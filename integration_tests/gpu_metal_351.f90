! Sections passed to a device function whose bounds read the index of a
! loop nested in the offloaded one, or a value an IF tests. Each section is
! gathered next to the statement that uses it, inside those loops and
! branches. Also: a run-time extent that does not change with the
! iteration, which sizes the buffer exactly.
module gpu_metal_351_mod
implicit none
contains

pure real function row_sum(x)
    real, intent(in) :: x(:)
    row_sum = sum(x)
end function

end module

program gpu_metal_351
use gpu_metal_351_mod
implicit none
real, allocatable :: a(:,:)
real :: s(3,3), t(3,3), x(3), v(3), w(3), r(3)
integer :: i, j, k, l, lo, hi
allocate(a(3,5))
do k = 1, 3
    do l = 1, 5
        a(k,l) = 10 * k + l
    end do
end do

! Extents that read the index of an inner do concurrent.
do concurrent (i = 1:3)
    x(i) = i
    do concurrent (j = 1:3)
        s(i,j) = row_sum(a(i,1:j)) + x(i)
        t(i,j) = row_sum(a(i,j:j+1))
    end do
end do
print *, s
print *, t
do k = 1, 3
    do l = 1, 3
        if (s(k,l) /= sum(a(k,1:l)) + k) error stop
        if (t(k,l) /= sum(a(k,l:l+1))) error stop
    end do
end do

! The same with an inner do.
s = 0
t = 0
do concurrent (i = 1:3)
    do j = 1, 3
        s(i,j) = row_sum(a(i,1:j))
        t(i,j) = row_sum(a(i,j:j+1))
    end do
end do
print *, s
print *, t
do k = 1, 3
    do l = 1, 3
        if (s(k,l) /= sum(a(k,1:l))) error stop
        if (t(k,l) /= sum(a(k,l:l+1))) error stop
    end do
end do

! A section that is only valid in the branch that uses it.
do concurrent (i = 1:3)
    if (2 * i <= 3) then
        v(i) = row_sum(a(i,1:2*i))
    else
        v(i) = row_sum(a(i,2*i-3:2*i-2))
    end if
end do
print *, v
if (any(v /= [23., 43., 67.])) error stop

! A section in the test of an IF, and others in its branches.
do concurrent (i = 1:3)
    if (row_sum(a(i,2:3)) > 50) then
        w(i) = row_sum(a(i,1:i+2))
    else
        w(i) = row_sum(a(i,i:i+2))
    end if
end do
print *, w
if (any(w /= [36., 69., 165.])) error stop

! A run-time extent that is the same in every iteration.
lo = 2
hi = 4
do concurrent (i = 1:3)
    r(i) = row_sum(a(i,lo:hi))
end do
print *, r
do k = 1, 3
    if (r(k) /= sum(a(k,lo:hi))) error stop
end do
end program
