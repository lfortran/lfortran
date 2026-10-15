module forall_07_mod
implicit none
contains
pure function cumulative_sum(x) result(y)
real, intent(in) :: x(:)
real :: y(size(x))
integer :: i
if (size(x) == 0) return
y(1) = x(1)
do i = 2, size(x)
    y(i) = y(i-1) + x(i)
end do
end function
end module

program forall_07
use forall_07_mod, only: cumulative_sum
implicit none
real :: a(3,5), s(3), x(3,2), y(4,2)
integer :: j, k, l

! A section bounded by the FORALL index is formed in each iteration.
do k = 1, 3
    do l = 1, 5
        a(k,l) = 10 * k + l
    end do
end do
forall (j = 1:3) s(j) = sum(a(1,1:j))
print *, s
if (any(s /= [11.0, 23.0, 36.0])) error stop 1

! An array-valued call in an array constructor assigned to a section.
x(:,1) = [1.0, 2.0, 3.0]
x(:,2) = [4.0, 5.0, 6.0]
forall (j = 1:size(x,2)) y(:,j) = [0.0, cumulative_sum(x(:,j))]
print *, y(:,1)
print *, y(:,2)
if (any(y(:,1) /= [0.0, 1.0, 3.0, 6.0])) error stop 2
if (any(y(:,2) /= [0.0, 4.0, 9.0, 15.0])) error stop 3
end program
