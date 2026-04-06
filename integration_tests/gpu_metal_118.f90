program gpu_metal_118
! Test: pure function returning array (size(x)) inside do concurrent
! Verifies that array-returning functions called with array section
! arguments inside do concurrent are correctly compiled for Metal GPU.
implicit none
real :: a(3,2), z(3,2)
integer :: i
z(:,1) = [1.0, 2.0, 3.0]
z(:,2) = [4.0, 5.0, 6.0]
a = 0.0

do concurrent (i = 1:2)
    a(:,i) = copy_vec(z(:,i))
end do

if (abs(a(1,1) - 1.0) > 1e-6) error stop
if (abs(a(2,1) - 2.0) > 1e-6) error stop
if (abs(a(3,1) - 3.0) > 1e-6) error stop
if (abs(a(1,2) - 4.0) > 1e-6) error stop
if (abs(a(2,2) - 5.0) > 1e-6) error stop
if (abs(a(3,2) - 6.0) > 1e-6) error stop
print *, "ok"

contains
    pure function copy_vec(x) result(y)
        real, intent(in) :: x(:)
        real :: y(size(x))
        y = x
    end function
end program
