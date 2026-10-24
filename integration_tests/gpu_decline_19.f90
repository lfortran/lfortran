! gpu decline reason: LocalityAssociateNotInlined
! An array of LOCAL locality is a new array in each iteration, which the
! kernel makes a local of a BLOCK around the loop body. An ASSOCIATE inside
! an IF of the body is not one the kernel inlines into that BLOCK, so the
! array cannot be given a copy per thread, and the loop is not offloaded.
program gpu_decline_19
implicit none
integer, parameter :: n = 1000
real :: x(n), g(n), t(2)
integer :: j
do j = 1, n
    x(j) = real(j)
end do
do concurrent (j = 1:n) local(t)
    if (j > 0) then
        associate (e => 2.0 * x(j))
            t(1) = x(j)
            t(2) = e
            g(j) = t(1) + t(2)
        end associate
    end if
end do
do j = 1, n
    if (abs(g(j) - 3.0 * x(j)) > 1.0e-3) error stop
end do
print *, "ok"
end program
