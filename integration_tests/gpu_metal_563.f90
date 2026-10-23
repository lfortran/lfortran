! A named cycle of an offloaded do concurrent from a loop inside it ends
! that iteration, as an unnamed cycle in the body does.
! Not registered for llvm: plain LLVM cannot find a do concurrent by name.
program gpu_metal_563
implicit none
integer :: s(4), i, j
s = 0
dc: do concurrent (i = 1:4)
    do j = 1, 5
        if (j > i) cycle dc
        s(i) = s(i) + j
    end do
    s(i) = -1
end do dc
print *, s
if (any(s /= [1, 3, 6, 10])) error stop
end program
