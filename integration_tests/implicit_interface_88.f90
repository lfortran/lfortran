! Units that use a module whose procedures reference procedures with implicit
! interfaces declared in another module.
program implicit_interface_88
    use implicit_interface_88_c
    implicit none
    real :: x
    x = 1.0
    call ii88_drive(x)
    if (abs(x - 5.0) > 1e-6) error stop 1
    call ii88_drive2(x)
    if (abs(x - 42.0) > 1e-6) error stop 2
    print *, x
end program

subroutine ii88_sub(x)
    real, intent(inout) :: x
    x = 2*x
end subroutine

real function ii88_fun(x)
    real, intent(in) :: x
    ii88_fun = x + 1
end function
