module operator_overloading_20_mod
    implicit none

    type :: Vector2D
        real :: x, y
    contains
        procedure :: add
        generic :: operator(+) => add
    end type Vector2D

contains
    type(Vector2D) function add(this, that)
        class(Vector2D), intent(in) :: this, that
        add%x = this%x + that%x
        add%y = this%y + that%y
    end function add
end module operator_overloading_20_mod

program operator_overloading_20
    use operator_overloading_20_mod
    implicit none
    type(Vector2D) :: v1, v2, v3

    v1 = Vector2D(1.0, 2.0)
    v2 = Vector2D(3.0, 4.0)
    v3 = v1 + v2

    if (abs(v3%x - 4.0) > 1e-5) error stop
    if (abs(v3%y - 6.0) > 1e-5) error stop

end program operator_overloading_20
