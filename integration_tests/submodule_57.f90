module submodule_57_c
    implicit none
    integer, parameter :: np = 3
end module submodule_57_c

module submodule_57_m
    use submodule_57_c, only: np
    implicit none
    interface
        module subroutine s(a)
            integer, intent(inout) :: a(np)
        end subroutine s
    end interface
end module submodule_57_m

submodule (submodule_57_m) submodule_57_s
contains
    module procedure s
        a = 2*a
    end procedure s
end submodule submodule_57_s

program submodule_57
    ! A separate module procedure whose dummy argument bound is a named
    ! constant of another module
    use submodule_57_m
    implicit none
    integer :: a(3)
    a = 2
    call s(a)
    if (any(a /= 4)) error stop
    print *, "ok"
end program submodule_57
