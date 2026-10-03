program gpu_metal_33
! Test: associate variable referencing derived-type array component
! inside do concurrent. Verifies that the gpu_offload pass correctly
! resolves associate(n => v%x) when lowering to a GPU kernel.
implicit none
type :: t
    integer :: x(4)
end type
type(t) :: v
integer :: i

v%x = 0
associate(n => v%x)
    do concurrent (i = 1:4)
        n(i) = i * 10
    end do
end associate

if (v%x(1) /= 10) error stop
if (v%x(2) /= 20) error stop
if (v%x(3) /= 30) error stop
if (v%x(4) /= 40) error stop
end program
