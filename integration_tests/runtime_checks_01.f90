module runtime_checks_01_mod
implicit none
real :: a(8,4)
contains
subroutine init
a(:,1) = [1.,2.,3.,4.,5.,6.,7.,8.]
a(:,2) = [2.,3.,4.,5.,6.,7.,8.,9.]
a(:,3) = [3.,4.,5.,6.,7.,8.,9.,10.]
a(:,4) = [4.,5.,6.,7.,8.,9.,10.,11.]
end subroutine
end module

program runtime_checks_01
! Statements guarded by runtime checks (bounds, shape, allocation status)
! that all pass
use runtime_checks_01_mod, only: a, init
implicit none
integer :: i, j, n
real, allocatable :: b(:), c(:)
call init()
do j = 1, 4
    do i = 1, 8
        if (abs(a(i,j) - real(i + j - 1)) > 1e-6) error stop
    end do
end do
n = 8
allocate(b(n), c(n))
b = a(:,2)
c = b + a(:,3)
do i = 1, n
    if (abs(c(i) - real(2*i + 3)) > 1e-6) error stop
end do
call check_sum(c, n, 96.0)
print *, sum(a), sum(c)
if (abs(sum(a) - 192.0) > 1e-6) error stop

contains

subroutine check_sum(x, m, expected)
integer, intent(in) :: m
real, intent(in) :: x(m), expected
if (abs(sum(x(1:m)) - expected) > 1e-6) error stop
end subroutine
end program
