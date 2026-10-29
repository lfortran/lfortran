program gpu_derived_component_21
    implicit none
    type tensor
        real, allocatable :: v(:)
        real, allocatable :: w(:)
    end type
    type(tensor) :: a(1)
    real :: x(2) = [1., 2.]
    integer :: i

    do concurrent (i = 1:1)
        a(i) = twice(x)
    end do
    if (any(a(1)%v /= x)) error stop 1
    if (any(a(1)%w /= x)) error stop 2
contains
    pure function twice(x) result(r)
        real, intent(in) :: x(:)
        type(tensor) :: r
        r%v = x
        r%w = x
    end function
end program
