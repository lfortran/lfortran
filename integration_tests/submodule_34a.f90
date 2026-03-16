program submodule_34
    use submodule_34_parent_mod
    implicit none
    integer :: x
    call test_sub(x)
    if (x /= 5) error stop
    print *, "ok"
end program submodule_34
