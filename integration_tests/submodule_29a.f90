program submodule_29
    use submodule_29_mod, only: greet
    implicit none
    integer :: x, y

    x = 3
    call greet(x, y)
    if (y /= 15) error stop
    print *, "ok"
end program
