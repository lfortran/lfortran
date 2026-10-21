module json_loc_module_m
! Used by json_loc_use_module.f90: the `dimension` of `x` and the
! `array_index` of `x(1)` are read back from this module's modfile, and their
! `loc` must point into this file.
integer, allocatable :: x(:)
contains
subroutine s()
x(1) = 0
end subroutine
end module
