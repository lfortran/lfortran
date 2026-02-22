module class_119_mod
    implicit none

    type :: divergence_1D_t
    contains
        procedure, pass(divergence_1D) :: f
    end type

    interface
        module function f(divergence_1D) result(r)
            class(divergence_1D_t), intent(in) :: divergence_1D
            integer :: r
        end function
    end interface
contains
    module function f(divergence_1D) result(r)
        class(divergence_1D_t), intent(in) :: divergence_1D
        integer :: r
        r = 4
    end function
end module

program class_119
    use class_119_mod, only: divergence_1D_t
    implicit none

    type(divergence_1D_t) :: d

    if (d%f() /= 4) error stop
    print *, "PASS"
end program
