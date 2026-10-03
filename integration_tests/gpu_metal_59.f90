program gpu_metal_59
! Test: block construct inside do loop inside do concurrent
! This used to fail with: "Block block should resolve in current scope."
! in the gpu_offload pass.
implicit none
integer :: i, l
integer :: x(4)
x = 0
do concurrent (i = 1:4)
    do l = 1, 1
        block
            x(i) = x(i) + i * l
        end block
    end do
end do
print *, x
if (x(1) /= 1) error stop
if (x(2) /= 2) error stop
if (x(3) /= 3) error stop
if (x(4) /= 4) error stop
end program
