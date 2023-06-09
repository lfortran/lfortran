module modules_51_module
    public :: a
end module

subroutine sub()
    use modules_51_module
    if (abs(a - 2.0) > 1e-8) error stop
    print *, a
end subroutine

program modules_51
    use modules_51_module
    a = 2
    call sub()
end program

