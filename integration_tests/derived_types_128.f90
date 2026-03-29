module derived_types_128_mod
    implicit none

    type :: point
        real :: x, y
    contains
        procedure, pass(self) :: add_default
        procedure, pass(pt) :: add_named
    end type point

contains

    function add_default(self, other) result(res)
        class(point), intent(in) :: self, other
        type(point) :: res
        res%x = self%x + other%x
        res%y = self%y + other%y
    end function

    function add_named(scale, pt) result(res)
        real, intent(in) :: scale
        class(point), intent(in) :: pt
        type(point) :: res
        res%x = pt%x * scale
        res%y = pt%y * scale
    end function

end module derived_types_128_mod

program derived_types_128
    use derived_types_128_mod
    implicit none

    type(point) :: p1, p2, result

    p1 = point(1.0, 2.0)
    p2 = point(3.0, 4.0)

    ! pass(self) where self is the first argument (default position)
    result = p1%add_default(p2)
    if (abs(result%x - 4.0) > 1e-6) error stop
    if (abs(result%y - 6.0) > 1e-6) error stop

    ! pass(pt) where pt is the second argument
    ! p1%add_named(10.0) should pass: add_named(10.0, p1)
    ! i.e., scale=10.0 and pt=p1
    result = p1%add_named(10.0)
    if (abs(result%x - 10.0) > 1e-6) error stop
    if (abs(result%y - 20.0) > 1e-6) error stop

    print *, "All tests passed."

end program derived_types_128
