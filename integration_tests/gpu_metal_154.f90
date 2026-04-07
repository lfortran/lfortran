program gpu_metal_154
! Test: ArrayBound UBound for assumed-shape 2D array inside do concurrent.
! Verifies that array section x(:,i) on a 2D assumed-shape argument
! correctly resolves per-dimension sizes in GPU kernels.
implicit none
real :: x(3, 2)
integer :: i, j
x = 2.0
call compute(x)
! After compute, x should be unchanged (copy to self)
do j = 1, 2
    do i = 1, 3
        if (abs(x(i,j) - 2.0) > 1.0e-6) error stop
    end do
end do

! Test with a different value
x = 5.0
call compute(x)
do j = 1, 2
    do i = 1, 3
        if (abs(x(i,j) - 5.0) > 1.0e-6) error stop
    end do
end do
print *, "ok"
contains
subroutine compute(x)
    real, intent(inout) :: x(:,:)
    integer :: i
    do concurrent (i = 1:2)
        x(:,i) = x(:,i)
    end do
end subroutine
end program
