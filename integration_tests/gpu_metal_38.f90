program gpu_metal_38
! Test: associate inside block inside do concurrent.
! The gpu_offload pass must descend into AssociateBlock bodies to
! collect and remap variable references correctly.
implicit none
integer :: n, pair, res(2)
n = 3
res = 0
do concurrent (pair = 1:2)
    block
        associate(nh => n)
            res(pair) = nh + pair
        end associate
    end block
end do
if (res(1) /= 4) error stop
if (res(2) /= 5) error stop
print *, "ok"
end program
