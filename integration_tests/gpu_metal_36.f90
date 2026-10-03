program gpu_metal_36
! Test: VLA (variable-length array) in block inside do concurrent
! The VLA dimension depends on an outer-scope variable, exercising the
! gpu_offload pass's handling of block-local variable type remapping.
implicit none
integer :: n, i, x(4)
n = 4
x = 0

do concurrent (i = 1:4)
    block
        real :: a(n)
        a(1) = real(i * 3)
        x(i) = int(a(1))
    end block
end do

if (x(1) /= 3) error stop
if (x(2) /= 6) error stop
if (x(3) /= 9) error stop
if (x(4) /= 12) error stop
print *, "ok"
end program
