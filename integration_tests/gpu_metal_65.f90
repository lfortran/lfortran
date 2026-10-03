program gpu_metal_65
! Test: do concurrent calling a contained function that references
! a host-scope Parameter (constant) variable. The gpu_offload pass
! must clone the Parameter into the duplicated function's scope.
implicit none
real, parameter :: val = 2.5
real :: y(4)
integer :: i

do concurrent(i=1:4)
    y(i) = bar()
end do

do i = 1, 4
    if (abs(y(i) - 2.5) > 1.0e-6) error stop
end do
print *, "ok"

contains
    pure function bar() result(b)
        real :: b
        b = val
    end function
end program
