program gpu_metal_66
! Test: do concurrent calling a contained function that itself calls
! another contained function. The gpu_offload pass must transitively
! duplicate both functions into the kernel scope.
implicit none
real :: y(4)
integer :: i

do concurrent(i=1:4)
    y(i) = f(real(i))
end do

do i = 1, 4
    if (abs(y(i) - real(i)) > 1.0e-6) error stop
end do
print *, "ok"

contains
    pure real function g(x)
        real, intent(in) :: x
        g = x
    end function
    pure real function f(x)
        real, intent(in) :: x
        f = g(x)
    end function
end program
