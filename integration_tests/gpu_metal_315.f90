! An array of a derived type of a rank above one is offloaded: the elements
! are handed to the kernel by their column-major position, so the kernel
! addresses p(i,2,1,2) through the same linearization the host laid out.
program gpu_metal_315
implicit none
type :: point_t
    real :: v
end type
integer :: i, j, k, m
type(point_t) :: p(2, 2, 2, 2)
real :: r(2)

do m = 1, 2
    do k = 1, 2
        do j = 1, 2
            do i = 1, 2
                p(i, j, k, m)%v = real(i + 2 * j + 4 * k + 8 * m)
            end do
        end do
    end do
end do

do concurrent (i = 1:2)
    r(i) = p(i, 2, 1, 2)%v * 2.0
end do

if (abs(r(1) - 50.0) > 1.0e-5) error stop
if (abs(r(2) - 52.0) > 1.0e-5) error stop

do concurrent (i = 1:2)
    p(i, 1, 2, 1)%v = real(100 * i)
end do

if (abs(p(1, 1, 2, 1)%v - 100.0) > 1.0e-5) error stop
if (abs(p(2, 1, 2, 1)%v - 200.0) > 1.0e-5) error stop

print *, "PASS"
end program
