! NOPASS type-bound subroutines called in a `do concurrent` through an
! `intent(in)` dummy argument, of a type with an array component and of a
! polymorphic one: the object is not passed to the procedure, so the loop
! does not define it, and the device reads it in place.
module gpu_liveout_14_m
    implicit none
    type :: model_t
        real :: coef(3)
        real :: s
    contains
        procedure, nopass :: eval
    end type
    type :: poly_t
        real :: c1
        real :: s
    contains
        procedure, nopass :: eval => eval_poly
    end type
contains
    pure subroutine eval(x, y)
        real, intent(in) :: x
        real, intent(out) :: y
        y = 2 * x
    end subroutine

    pure subroutine eval_poly(x, y)
        real, intent(in) :: x
        real, intent(out) :: y
        y = 2 * x
    end subroutine

    subroutine run(m, x, y)
        type(model_t), intent(in) :: m
        real, intent(in) :: x(:)
        real, intent(out) :: y(:)
        integer :: i
        do concurrent (i = 1:size(x))
            call m%eval(x(i), y(i))
            y(i) = y(i) + m%s + m%coef(2)
        end do
    end subroutine

    subroutine run_poly(m, x, y)
        class(poly_t), intent(in) :: m
        real, intent(in) :: x(:)
        real, intent(out) :: y(:)
        integer :: i
        do concurrent (i = 1:size(x))
            call m%eval(x(i), y(i))
            y(i) = y(i) + m%s + m%c1
        end do
    end subroutine
end module

program gpu_liveout_14
    use gpu_liveout_14_m
    implicit none
    integer, parameter :: n = 100
    type(model_t) :: m
    type(poly_t) :: p
    real :: x(n), y(n)
    integer :: i
    m%coef = [1., 2., 3.]
    m%s = 10
    p%c1 = 2
    p%s = 10
    do i = 1, n
        x(i) = i
    end do
    call run(m, x, y)
    print *, y(7)
    if (y(7) /= 26) error stop 1
    call run_poly(p, x, y)
    print *, y(7)
    if (y(7) /= 26) error stop 2
end program
