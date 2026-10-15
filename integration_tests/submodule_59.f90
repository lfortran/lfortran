! A separate module procedure with a dummy procedure declared by an inline
! interface block whose explicit-shape dummy has a use-associated extent
! (#14472)
module submodule_59_m1
    implicit none
    integer :: n = 2
end module

module submodule_59_m2
    implicit none
    interface
        module subroutine s(p, x)
            interface
                subroutine p(x)
                    use submodule_59_m1, only: n
                    real :: x(n)
                end subroutine
            end interface
            real :: x(2)
        end subroutine
    end interface
contains
    subroutine fill(x)
        use submodule_59_m1, only: n
        real :: x(n)
        x = 7
    end subroutine
end module

submodule (submodule_59_m2) submodule_59_m2s
    implicit none
contains
    module procedure s
        call p(x)
    end procedure
end submodule

program submodule_59
    use submodule_59_m2
    implicit none
    real :: x(2)
    x = 0
    call s(fill, x)
    if (any(x /= 7)) error stop
    print *, x
end program
