program gpu_metal_23
! Test: do concurrent inside associate block compiles and runs with GPU offloading
implicit none
integer, parameter :: n = 1000
integer :: a(n), b(n), i

do i = 1, n
    a(i) = i
    b(i) = 0
end do

associate(aa => a, bb => b)
    do concurrent (i = 1:n)
        bb(i) = aa(i) * 2
    end do
end associate

do i = 1, n
    if (b(i) /= i * 2) error stop
end do

print *, "PASSED"
end program
