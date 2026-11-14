program pdt_modfile_parent_nondefault_02
    use pdt_nondefault_nested_child_m, only: child_t, reset
    implicit none
    type(child_t(8)) :: x
    if (x%k /= 8 .or. x%leaf%j /= 8) error stop 1
    if (kind(x%leaf%value) /= 8) error stop 2
    x%parent_default = 23
    if (x%parent_default /= 23) error stop 3
    call reset(x)
end program
