! Procedures accessed through a namespace used as actual arguments,
! procedure pointer targets, and interfaces in procedure declarations.
module namespace_modules_09_funcs
    implicit none
    abstract interface
        real function real_func(x)
            real, intent(in) :: x
        end function
    end interface
contains
    real function square(x)
        real, intent(in) :: x
        square = x*x
    end function

    real function cube(x)
        real, intent(in) :: x
        cube = x*x*x
    end function

    real function apply(f, x)
        procedure(real_func) :: f
        real, intent(in) :: x
        apply = f(x)
    end function
end module

program namespace_modules_09
    use, namespace :: fn => namespace_modules_09_funcs
    implicit none
    procedure(fn%real_func), pointer :: p => null()

    ! Actual argument
    if (abs(fn%apply(fn%square, 3.0) - 9.0) > 1e-6) error stop
    if (abs(fn%apply(fn%cube, 2.0) - 8.0) > 1e-6) error stop
    if (abs(total(fn%square, 2.0) - 8.0) > 1e-6) error stop

    ! Procedure pointer target
    p => fn%cube
    if (.not. associated(p)) error stop
    if (.not. associated(p, fn%cube)) error stop
    if (abs(p(3.0) - 27.0) > 1e-5) error stop
    p => fn%square
    if (abs(fn%apply(p, 5.0) - 25.0) > 1e-5) error stop
    print *, p(4.0)
contains
    real function total(f, x)
        procedure(fn%real_func) :: f
        real, intent(in) :: x
        total = f(x) + f(x)
    end function
end program
