module json_loc_template_module_m
! Used by json_loc_template_use_module.f90: instantiating the template copies
! the `array_index` of `lst(1)`, and the copy saved in this module's modfile
! must keep the location of `lst(1)` in this file.
implicit none
private
public :: first_int

requirement r {t}
    deferred type :: t
end requirement

template first_tmpl {t}
    require :: r {t}
    private
    public :: first_t
contains
    function first_t(lst) result(res)
        type(t), intent(in) :: lst(:)
        type(t) :: res
        res = lst(1)
    end function
end template

instantiate first_tmpl {integer}, only: first_int => first_t
end module
