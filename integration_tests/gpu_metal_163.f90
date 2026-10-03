! Test: sum() inside if-guard within do concurrent GPU offload.
! Previously, the GPU offload pass did not recurse into If statement
! bodies when inlining sum(), causing a call to _lcompilers_Sum
! (unavailable in Metal shader).
program gpu_metal_163
implicit none
integer :: pair
real :: vals(3, 4), pair_cost(4)

vals = 0.5

do concurrent (pair = 1:4)
    if (.true.) pair_cost(pair) = sum(vals(:, pair))
end do

do pair = 1, 4
    if (abs(pair_cost(pair) - 1.5) > 1.0e-5) error stop
end do
print *, "PASS"
end program
