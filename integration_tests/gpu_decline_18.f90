! gpu decline reason: LocalityNamedByAssociate
! An array of LOCAL locality is a new array in each iteration, but an
! associate name of the construct around the loop still names the array
! outside it. The kernel resolves that name to the array itself, so it
! cannot tell the two apart, and the loop is not offloaded.
program gpu_decline_18
implicit none
integer, parameter :: n = 1000
real :: x(n), g(n), t(2)
integer :: j
do j = 1, n
    x(j) = real(j)
end do
t = [10.0, 20.0]
associate (a => t)
    do concurrent (j = 1:n) local(t)
        t(1) = x(j)
        t(2) = a(2)
        g(j) = t(1) + t(2)
    end do
end associate
do j = 1, n
    if (abs(g(j) - (x(j) + 20.0)) > 1.0e-3) error stop
end do
print *, "ok"
end program
