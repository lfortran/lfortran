program pdt_modfile_parent_nondefault_04
    use pdt_nondefault_nested_child_m, only: child_t
    implicit none
    type(child_t(8)) :: x

    if (x%parent_default /= 17) error stop 1
end program
