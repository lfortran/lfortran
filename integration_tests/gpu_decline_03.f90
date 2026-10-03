! gpu decline reason: DeviceFunctionImplementation
! the loop body calls an external procedure that is only declared by an
! interface block, so there is no body to compile for the device.
program gpu_decline_03
    implicit none
    interface
        pure real function gpu_decline_03_twice(x)
            real, intent(in) :: x
        end function
    end interface
    real :: a(4)
    integer :: i
    do concurrent (i = 1:4)
        a(i) = gpu_decline_03_twice(real(i))
    end do
    do i = 1, 4
        if (abs(a(i) - 2.0 * real(i)) > 1.0e-6) error stop "bad a"
    end do
end program
