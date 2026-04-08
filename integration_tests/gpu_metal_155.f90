program gpu_metal_155
! Test assumed-shape multi-dimensional array indexing in do concurrent.
! Verifies that column-major strides are computed correctly so that
! x(1,2) is actually the second-column element, not x(1,1).
implicit none
real :: x(2,3)
integer :: i

x = 0.0
x(1,2) = 5.0
x(2,3) = 10.0

call add_one(x)

if (abs(x(1,2) - 6.0) > 1.0e-6) error stop
if (abs(x(2,3) - 11.0) > 1.0e-6) error stop
if (abs(x(1,1) - 0.0) > 1.0e-6) error stop
print *, "ok"

contains

subroutine add_one(a)
    real, intent(inout) :: a(:,:)
    integer :: i
    do concurrent (i = 1:1)
        a(1,2) = a(1,2) + 1.0
        a(2,3) = a(2,3) + 1.0
    end do
end subroutine

end program
