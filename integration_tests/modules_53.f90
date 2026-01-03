module module_modules_53
    real :: x(5)
end module

subroutine check_x()
    use module_modules_53, only: x
    print *, x
    if (any(abs(x - 1.39) > 1e-9)) error stop
    x = -2.91
end subroutine

program modules_53
    use module_modules_53, only: x
    x = 1.39
    call check_x()
    print *, x
    if (any(abs(x - (-2.91)) > 1e-9)) error stop
end program
