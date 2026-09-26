! A procedure or derived type instantiated in a module gets the accessibility
! of that module, not the default accessibility of the module that defines the
! template.
module template_access_01_tmpl_m
    implicit none
    private
    public :: t_tmpl

    template t_tmpl {T}
        deferred type :: T
        type :: box
            type(T) :: v
        end type
    contains
        function f(x) result(r)
            type(T), intent(in) :: x
            type(T) :: r
            r = x
        end function

        function mk(x) result(r)
            type(T), intent(in) :: x
            type(box) :: r
            r%v = x
        end function
    end template
end module

module template_access_01_inst_m
    use template_access_01_tmpl_m, only: t_tmpl
    implicit none
    instantiate t_tmpl {real}, only: g => f, rbox => box, rmk => mk
end module

module template_access_01_priv_m
    use template_access_01_tmpl_m, only: t_tmpl
    implicit none
    private
    public :: h
    instantiate t_tmpl {integer}, only: h => f
end module

program template_access_01
    use template_access_01_inst_m
    use template_access_01_priv_m
    implicit none
    type(rbox) :: b
    b = rmk(2.5)
    if (abs(b%v - 2.5) > 1e-6) error stop
    if (abs(g(3.0) - 3.0) > 1e-6) error stop
    if (h(7) /= 7) error stop
    print *, g(3.0), h(7), b%v
end program
