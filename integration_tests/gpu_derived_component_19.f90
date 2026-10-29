program gpu_derived_component_19
    implicit none
    type t
        real, allocatable :: v(:)
    end type
    type(t) :: a(1)
    real :: x(1)
    integer :: i

    do concurrent (i = 1:1)
        a(i) = f()
    end do
    if (size(a(1)%v) /= 1) error stop
    if (a(1)%v(1) /= 1.) error stop
    x = [2.]
    do concurrent (i = 1:1)
        a(i) = g(x)
    end do
    if (size(a(1)%v) /= 1) error stop
    if (a(1)%v(1) /= 2.) error stop
contains
    pure function f() result(r)
        type(t) :: r
        r%v = [1.]
    end function

    pure function g(x) result(r)
        real, intent(in) :: x(:)
        type(t) :: r
        r%v = x
    end function
end program
