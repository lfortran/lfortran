module template_struct_03_m
    implicit none

    template tm {t}
        deferred type :: t
        type :: pair
            type(t) :: a
        contains
            procedure :: set
            procedure :: get
        end type
    contains
        subroutine set(this, value)
            class(pair), intent(inout) :: this
            type(t), intent(in) :: value
            this%a = value
        end subroutine

        function get(this) result(r)
            class(pair), intent(in) :: this
            type(t) :: r
            r = this%a
        end function

        function first(x, y) result(r)
            type(t), intent(in) :: x, y
            type(t) :: r
            type(pair) :: p
            p%a = x
            call p%set(y)
            r = p%a
        end function

        function read_back(x) result(r)
            type(t), intent(in) :: x
            type(t) :: r
            type(pair) :: p
            p%a = x
            r = p%get()
        end function

        function through_helper(x, y) result(r)
            type(t), intent(in) :: x, y
            type(t) :: r
            r = first(x, y)
        end function
    end template
end module

program template_struct_03
    use template_struct_03_m
    implicit none
    instantiate tm {integer}, only: ifirst => first, iread => read_back
    instantiate tm {real}, only: rfirst => first, rread => read_back
    instantiate tm {integer}, only: explicit_pair => pair, explicit_first => first
    type(explicit_pair) :: p

    if (ifirst(0, 17) /= 17) error stop
    if (ifirst(6, -11) /= -11) error stop
    if (iread(23) /= 23) error stop
    if (iread(-9) /= -9) error stop
    if (abs(rfirst(0.0, 1.5) - 1.5) > 1e-6) error stop
    if (abs(rread(-2.5) + 2.5) > 1e-6) error stop
    if (explicit_first(0, 31) /= 31) error stop
    p%a = 0
    call p%set(42)
    if (p%a /= 42) error stop
    if (p%get() /= 42) error stop
    call check_helper()
contains
    subroutine check_helper()
        instantiate tm {integer}, only: hidden_first => through_helper
        if (hidden_first(0, 53) /= 53) error stop
    end subroutine
end program
