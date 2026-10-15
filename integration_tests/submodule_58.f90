! A separate module procedure with a dummy procedure whose interface has an
! explicit-shape dummy with a use-associated extent (#14472)
module submodule_58_m1
    implicit none
    integer :: n = 2
end module

module submodule_58_m2
    implicit none
    abstract interface
        subroutine cb(x)
            use submodule_58_m1, only: n
            real :: x(n)
        end subroutine
    end interface
    interface
        module subroutine s(p, x)
            procedure(cb) :: p
            real :: x(2)
        end subroutine
    end interface
contains
    subroutine fill(x)
        use submodule_58_m1, only: n
        real :: x(n)
        x = 7
    end subroutine
end module

submodule (submodule_58_m2) submodule_58_m2s
    implicit none
contains
    module procedure s
        call p(x)
    end procedure
end submodule

program submodule_58
    use submodule_58_m2
    implicit none
    real :: x(2)
    x = 0
    call s(fill, x)
    if (any(x /= 7)) error stop
    print *, x
end program
