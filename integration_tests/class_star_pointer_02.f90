module class_star_pointer_02_m
    implicit none
    class(*), pointer :: mu => null()
end module

program class_star_pointer_02
    use class_star_pointer_02_m, only: mu
    implicit none
    integer, target :: x
    real, target :: r
    class(*), pointer :: u => null()

    if (associated(u)) error stop "u"
    if (associated(mu)) error stop "mu"

    x = 5
    u => x
    if (.not. associated(u)) error stop "u => x"
    select type (u)
    type is (integer)
        if (u /= 5) error stop "u value"
    class default
        error stop "u type"
    end select

    r = 2.5
    mu => r
    if (.not. associated(mu)) error stop "mu => r"
    u => mu
    select type (u)
    type is (real)
        if (abs(u - 2.5) > 1e-6) error stop "mu value"
    class default
        error stop "mu type"
    end select

    call check_local()
    print *, "PASS"

contains

    subroutine check_local()
        class(*), pointer :: p => null()
        if (associated(p)) error stop "p"
        p => x
        if (.not. associated(p, x)) error stop "p => x"
    end subroutine

end program
