! A section passed to a procedure where it is evaluated more than once, or
! only under a condition -- a FORALL assignment, a DO WHILE condition, an
! arm of a conditional expression -- is copied into a contiguous buffer
! before the statement when nothing it reads changes there. The copy is
! guarded by the condition, so a section that exists only where the call
! is evaluated is not read elsewhere.
module gpu_metal_353_mod
implicit none
contains
pure real function row_sum(x)
    real, intent(in) :: x(:)
    row_sum = sum(x)
end function
end module

program gpu_metal_353
use gpu_metal_353_mod
implicit none
real :: a(3,5), s(3,3), t(3,3), q(3,4), y(3,3), z(3,3), v(3), w(3)
real, allocatable :: e(:,:)
integer :: i, j, k, l, m, r, n(3), p(3)
real :: expect
do k = 1, 3
    do l = 1, 5
        a(k,l) = 10 * k + l
    end do
end do
allocate(e(3,5))
e = a
s = 0; t = 0; q = 0; y = 0; z = -7; v = 0; w = 0; n = 0; p = 0

do concurrent (i = 1:3)
    forall (j = 1:3) s(i,j) = row_sum(a(i,:)) * j
    forall (j = 1:3) t(i,j) = row_sum(a(i,1:5:2)) + j
    forall (j = 1:2, l = 1:2) q(i,2*j+l-2) = row_sum(a(i,2:5:2)) * j + l
    forall (j = 1:3) y(i,j) = (i /= 2 ? row_sum(a(i,:)) : -1.0) * j
    ! Runs no times; a(3,1:6) does not exist.
    forall (j = 1:i-3) z(i,j) = row_sum(a(i,1:2*i))
    k = 0
    do while (row_sum(a(i,:)) * k < 200)
        k = k + 1
    end do
    n(i) = k
    m = 0
    do while (row_sum(e(i,2:5)) * m < 100)
        m = m + 1
    end do
    p(i) = m
    v(i) = (i <= 2 ? row_sum(a(i,:)) : 0.0)
    ! a(3,1:6) does not exist, and that arm is not taken for i = 3.
    w(i) = (i <= 2 ? row_sum(a(i,1:2*i)) : 0.0)
end do

print *, s
print *, t
print *, q
print *, y
print *, n, p
print *, v, w
do r = 1, 3
    do j = 1, 3
        if (abs(s(r,j) - sum(a(r,:)) * j) > 1e-3) error stop 1
        if (abs(t(r,j) - (sum(a(r,1:5:2)) + j)) > 1e-3) error stop 2
        expect = sum(a(r,:)) * j
        if (r == 2) expect = -j
        if (abs(y(r,j) - expect) > 1e-3) error stop 3
        if (z(r,j) /= -7) error stop 4
    end do
    do j = 1, 2
        do l = 1, 2
            if (abs(q(r,2*j+l-2) - (sum(a(r,2:5:2)) * j + l)) > 1e-3) then
                error stop 5
            end if
        end do
    end do
end do
if (any(n /= [4, 2, 2])) error stop 6
if (any(p /= [2, 2, 1])) error stop 7
if (abs(v(1) - 65) > 1e-3 .or. abs(v(2) - 115) > 1e-3 .or. v(3) /= 0) then
    error stop 8
end if
if (abs(w(1) - 23) > 1e-3 .or. abs(w(2) - 90) > 1e-3 .or. w(3) /= 0) then
    error stop 9
end if
end program
