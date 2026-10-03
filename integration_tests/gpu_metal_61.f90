program gpu_metal_61
! Test: do concurrent with associate calling a function that itself
! contains an associate block. Verifies that the gpu_offload pass
! correctly duplicates functions containing AssociateBlocks into
! the kernel scope, and that the Metal backend handles array
! parameters and ArraySize in duplicated functions.
implicit none
integer :: arr(4), pair, res(1)
arr = [2, 3, 3, 1]
res = 0
do concurrent (pair = 1:1)
    associate(nh => get_count(arr))
        res(pair) = nh
    end associate
end do
if (res(1) /= 2) error stop
print *, "ok"
contains
    pure function get_count(arr) result(count)
        integer, intent(in) :: arr(:)
        integer :: count
        associate(n => size(arr))
            count = n - 2
        end associate
    end function
end program
