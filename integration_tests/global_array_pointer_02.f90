module global_array_pointer_02_mod
    implicit none
    integer, pointer :: p(:) => null()
contains
    subroutine init()
        allocate(p(-2:5))
        p = -1
        p(-2) = 7
    end subroutine
end module

program global_array_pointer_02
    use global_array_pointer_02_mod
    implicit none
    call init()
    if (lbound(p,1) /= -2) error stop "lb"
    if (ubound(p,1) /= 5) error stop "ub"
    if (p(-2) /= 7) error stop "v"
    if (p(0) /= -1) error stop "v0"
    print *, "PASS"
end program
