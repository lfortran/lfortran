! Generic interfaces declared in external procedures

module generic_interface_external_procedure_01_mod
    implicit none
    type :: counter_t
        integer :: k = 4
    contains
        procedure :: n => counter_n
    end type
contains
    pure integer function counter_n(self)
        class(counter_t), intent(in) :: self
        counter_n = self%k
    end function
end module

subroutine apply(x)
    interface
        integer function twice(y)
            integer, intent(in) :: y
        end function
    end interface
    interface g
        procedure twice
    end interface
    integer, intent(inout) :: x
    x = g(x)
end subroutine

integer function twice(y)
    integer, intent(in) :: y
    twice = 2*y
end function

! A bound that is a type-bound function call is evaluated at the same point.
integer function counted(obj)
    use generic_interface_external_procedure_01_mod
    implicit none
    type(counter_t), intent(in) :: obj
    integer :: w(obj%n())
    w = 2
    counted = sum(w)
end function

program generic_interface_external_procedure_01
    use generic_interface_external_procedure_01_mod
    implicit none
    interface
        integer function scaled(n)
            integer, intent(in) :: n
        end function
        integer function counted(obj)
            import :: counter_t
            type(counter_t), intent(in) :: obj
        end function
    end interface
    integer :: v
    type(counter_t) :: c
    v = 5
    call apply(v)
    if (v /= 10) error stop
    if (scaled(3) /= 11) error stop
    if (counted(c) /= 8) error stop
    print *, v, scaled(3), counted(c)
end program

! An external procedure that follows the main program. Its generic `g` is not
! the one of `apply`, and is also referenced in a specification expression.
integer function scaled(n)
    implicit none
    interface g
        pure integer function triple(y)
            integer, intent(in) :: y
        end function
        procedure half
    end interface
    integer, intent(in) :: n
    integer :: a(g(n))
    a = 1
    scaled = sum(a) + int(g(4.0))
contains
    real function half(y)
        real, intent(in) :: y
        half = y/2
    end function
end function

pure integer function triple(y)
    integer, intent(in) :: y
    triple = 3*y
end function
