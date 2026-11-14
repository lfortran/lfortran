module array_component_element_arg_01_m
    implicit none
    type :: box
        real :: v(3)
    end type
contains
    subroutine bump(x)
        real, intent(inout) :: x(:)
        x = x + 10.0
    end subroutine
    real function total(x)
        real, intent(in) :: x(:)
        total = sum(x)
    end function
    subroutine set_out(x)
        real, intent(out) :: x(4)
        x = [101.0, 102.0, 103.0, 104.0]
    end subroutine
end module

program array_component_element_arg_01
    ! An element of an array component of an array, boxes%v(2), is passed
    ! as an array made of that element of every element of boxes.
    use array_component_element_arg_01_m
    implicit none
    type(box) :: boxes(4)
    integer :: k
    do k = 1, 4
        boxes(k)%v = [real(k), 2.0*k, 3.0*k]
    end do
    call bump(boxes%v(2))
    if (total(boxes%v(3)) /= 30.0) error stop
    call set_out(boxes%v(1))
    do k = 1, 4
        if (boxes(k)%v(1) /= 100.0 + k) error stop
        if (boxes(k)%v(2) /= 2.0*k + 10.0) error stop
        if (boxes(k)%v(3) /= 3.0*k) error stop
    end do
    print *, boxes%v(2)
end program
