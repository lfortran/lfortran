module derived_types_129_mod
    implicit none

    type :: point
        real :: x, y
        procedure(add_default_iface), pointer, nopass :: add_default => null()
        procedure(add_named_iface), pointer, nopass :: add_named => null()
    end type point

    abstract interface
        function add_default_iface(self, other) result(res)
            import :: point
            class(point), intent(in) :: self, other
            type(point) :: res
        end function

        function add_named_iface(scale, pt) result(res)
            import :: point
            real, intent(in) :: scale
            class(point), intent(in) :: pt
            type(point) :: res
        end function
    end interface

contains

    function add_default_impl(self, other) result(res)
        class(point), intent(in) :: self, other
        type(point) :: res
        res%x = self%x + other%x
        res%y = self%y + other%y
    end function

    function add_named_impl(scale, pt) result(res)
        real, intent(in) :: scale
        class(point), intent(in) :: pt
        type(point) :: res
        res%x = pt%x * scale
        res%y = pt%y * scale
    end function

end module derived_types_129_mod

program derived_types_129
    use derived_types_129_mod
    implicit none

    type(point) :: p1, p2, result

    p1%x = 1.0
    p1%y = 2.0
    p1%add_default => add_default_impl
    p1%add_named => add_named_impl

    p2%x = 3.0
    p2%y = 4.0

    ! Call via procedure pointer: add_default(p1, p2)
    result = p1%add_default(p1, p2)
    if (abs(result%x - 4.0) > 1e-6) error stop
    if (abs(result%y - 6.0) > 1e-6) error stop

    ! Call via procedure pointer: add_named(10.0, p1)
    result = p1%add_named(10.0, p1)
    if (abs(result%x - 10.0) > 1e-6) error stop
    if (abs(result%y - 20.0) > 1e-6) error stop

    print *, "All tests passed."

end program derived_types_129
