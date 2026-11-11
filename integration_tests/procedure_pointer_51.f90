program procedure_pointer_51
    ! A procedure-pointer component whose interface bounds an argument
    ! with a passed-object component (`dimension(me%n)`) can be loaded
    ! from the module file and called.
    use procedure_pointer_51_mod
    implicit none
    type(t) :: a
    real :: x(2)
    a%n = 2
    a%f => set_one
    x = 0.0
    call a%f(x)
    if (any(x /= 1.0)) error stop 1
end program
