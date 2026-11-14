program pdt_modfile_parent_nondefault_03
    use pdt_nondefault_first_m, only: first_child
    use pdt_nondefault_second_m, only: second_child
    implicit none
    type(first_child(8)) :: first_eight
    type(second_child(8)) :: second_eight

    if (first_eight%parent_default /= 17 .or. &
        second_eight%parent_default /= 17) error stop 1
    if (first_eight%child_default /= 42 .or. &
        second_eight%child_default /= 73) error stop 2
end program
