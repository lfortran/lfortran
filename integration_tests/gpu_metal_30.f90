program gpu_metal_30
! Test: do concurrent inside a block with block-local variables
implicit none
integer :: i, res

res = 0

block
    integer :: x(5)
    do concurrent (i = 1:5)
        x(i) = i * 3
    end do
    res = x(1) + x(2) + x(3) + x(4) + x(5)
end block

if (res /= 45) error stop
print *, "ok"
end program
