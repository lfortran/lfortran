! A requirement may declare an abstract interface whose name is not a
! deferred argument (NOTE 2 of 16.6.1): it is a local entity of the
! requirement, used here as the interface of a deferred procedure.
module template_requirement_abstract_interface_01_m
    implicit none
    private
    public :: run

    requirement unary_r{t, g}
        deferred type :: t
        abstract interface
            function unary_i(x) result(res)
                import :: t
                type(t), intent(in) :: x
                type(t) :: res
            end function
        end interface
        deferred procedure (unary_i) :: g
    end requirement

    requirement binary_r{t, h}
        deferred type :: t
        abstract interface
            subroutine unused_i()
            end subroutine
        end interface
        deferred interface
            function h(x, y) result(res)
                type(t), intent(in) :: x, y
                type(t) :: res
            end function
        end interface
    end requirement

    template tmpl{t, g, h}
        require :: unary_r{t, g}
        require :: binary_r{t, h}
        private
        public :: apply
    contains
        function apply(x, y) result(res)
            type(t), intent(in) :: x, y
            type(t) :: res
            res = h(g(x), y)
        end function
    end template

contains

    integer function dbl(x) result(res)
        integer, intent(in) :: x
        res = 2*x
    end function

    integer function add(x, y) result(res)
        integer, intent(in) :: x, y
        res = x + y
    end function

    subroutine run()
        integer :: r
        instantiate tmpl{integer, dbl, add}, only: apply
        r = apply(20, 2)
        print *, r
        if (r /= 42) error stop
    end subroutine

end module

program template_requirement_abstract_interface_01
    use template_requirement_abstract_interface_01_m
    implicit none
    call run()
end program
