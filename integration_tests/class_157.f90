module class_157_mod
    implicit none

    type, abstract :: base_t
    contains
        procedure(apply_interface), deferred :: apply
    end type

    abstract interface
        subroutine apply_interface(this, a, n)
            import base_t
            class(base_t) :: this
            real :: a(n)
            integer, intent(in) :: n
        end subroutine
    end interface

    type, extends(base_t) :: child_t
    contains
        procedure :: apply
    end type

contains

    subroutine apply(this, a, n)
        class(child_t) :: this
        integer, intent(in) :: n
        real :: a(n)
        a = real(n)
    end subroutine

end module

program class_157
    use class_157_mod
    implicit none
    class(base_t), allocatable :: obj
    real :: x(3)

    allocate(child_t :: obj)
    x = 0.0
    call obj%apply(x, 3)
    print *, x
    if (any(abs(x - 3.0) > 1e-6)) error stop
end program
