program gpu_metal_49
! Test: associate-bound name inside nested blocks within do concurrent
implicit none
real :: arr(4, 3)
integer :: pair
arr = 1.0

associate(b => arr)
    do concurrent(pair = 1:2)
        block
            block
                b(1,pair) = 0.0
            end block
        end block
    end do
end associate

if (arr(1,1) /= 0.0) error stop
if (arr(1,2) /= 0.0) error stop
if (arr(2,1) /= 1.0) error stop
if (arr(1,3) /= 1.0) error stop
print *, "PASS"
end program
