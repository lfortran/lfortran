program submodule_30
    use submodule_30_mod, only: compute
    implicit none
    integer :: x, y

    x = 5
    call compute(x, y)
    if (y /= 35) error stop
    print *, "ok"
end program
