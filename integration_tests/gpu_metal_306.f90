! An array of derived type of rank 2 has no device element index.
! The launch used to commit and then throw in the device C emitter.
! The loop stays on the host.
program gpu_metal_306
implicit none
type :: t
    real :: x
end type
type(t) :: c(2, 2)
integer :: i, j

do concurrent (i = 1:2, j = 1:2)
    c(i, j)%x = real(10 * i + j)
end do

do i = 1, 2
    do j = 1, 2
        if (abs(c(i, j)%x - real(10 * i + j)) > 1.0e-5) error stop
    end do
end do

print *, "PASS"
end program
