module json_loc_template_module_m
! Used by json_loc_template_use_module.f90: instantiating the template copies
! the `array_index` of `x(1)`, and the copy read back from this module's
! modfile must keep the location of `x(1)` in this file.
integer, allocatable :: x(:)
template tm {t}
    deferred type :: t
contains
    subroutine s()
        x(1) = 0
    end subroutine
end template
instantiate tm {integer}, only: si => s
end module
