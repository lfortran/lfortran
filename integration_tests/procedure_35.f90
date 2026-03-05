module procedure_35_mod
    implicit none
    private

    abstract interface
        real function reduction_fn(x, y) result(r)
            real, intent(in) :: x(:)
            real, intent(in) :: y(:)
        end function
    end interface

    type, public :: linop_type
        procedure(reduction_fn), nopass, pointer :: inner_product => default_dot
    end type

contains

    real function default_dot(x, y) result(r)
        real, intent(in) :: x(:)
        real, intent(in) :: y(:)
        r = dot_product(x, y)
    end function

end module procedure_35_mod

program procedure_35
    use procedure_35_mod
    implicit none
    type(linop_type) :: op
    real :: x(3), y(3)
    x = [1.0, 2.0, 3.0]
    y = [4.0, 5.0, 6.0]
    if (abs(op%inner_product(x, y) - 32.0) > 1e-6) error stop
    print *, "ok"
end program procedure_35
