program gpu_metal_31
! Test: do concurrent inside associate inside block with block-local variables
implicit none
integer :: total

total = 0

block
    integer :: i, x(4)
    x = 0
    associate(n => 4)
        do concurrent(i = 1:n)
            x(i) = i * 2
        end do
    end associate
    total = x(1) + x(2) + x(3) + x(4)
end block

if (total /= 20) error stop
print *, "ok"
end program
