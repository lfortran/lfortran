program gpu_metal_46
! Test: fixed-size local array inside block in do concurrent,
! using element-wise operations. Exercises the gpu_offload pass's
! block processing with array variables.
implicit none
integer :: x(4), i
x = 0
do concurrent (i = 1:4)
    block
        integer :: a(4)
        a(1) = i * 5
        a(2) = 0
        a(3) = 0
        a(4) = 0
        x(i) = a(1)
    end block
end do
if (x(1) /= 5) error stop
if (x(2) /= 10) error stop
if (x(3) /= 15) error stop
if (x(4) /= 20) error stop
print *, "ok"
end program gpu_metal_46
