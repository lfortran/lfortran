! A procedure instantiated in a module gets the accessibility of that module,
! not the default accessibility of the module that defines the template.
module template_access_01_tmpl_m
    implicit none
    private
    public :: t_tmpl

    template t_tmpl {T}
        deferred type :: T
    contains
        function f(x) result(r)
            type(T), intent(in) :: x
            type(T) :: r
            r = x
        end function
    end template
end module

module template_access_01_inst_m
    use template_access_01_tmpl_m, only: t_tmpl
    implicit none
    instantiate t_tmpl {real}, only: g => f
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
    if (abs(g(3.0) - 3.0) > 1e-6) error stop
    if (h(7) /= 7) error stop
    print *, g(3.0), h(7)
end program
