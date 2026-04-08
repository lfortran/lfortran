! Test: sum() of array expression inside associate block within do concurrent.
! Previously, the GPU offload pass did not recurse into AssociateBlockCall
! when inlining sum(), causing a call to _lcompilers_Sum (unavailable in
! Metal shader) and "unknown array size" placeholders.
program gpu_metal_162
implicit none
integer :: i
real :: a(3), b(3), results(4)

a = [1.0, 2.0, 3.0]
b = [0.5, 0.5, 0.5]

do concurrent (i = 1:4)
    block
        associate(x => a, y => b)
            results(i) = sum((x - y)**2)
        end associate
    end block
end do

! sum((0.5^2 + 1.5^2 + 2.5^2)) = 0.25 + 2.25 + 6.25 = 8.75
do i = 1, 4
    if (abs(results(i) - 8.75) > 1.0e-5) error stop
end do
print *, "PASS"
end program
