program pdt_modfile_parent_nondefault_01
    use pdt_nondefault_first_m, only: first_child, reset_first
    use pdt_nondefault_second_m, only: second_child, reset_second
    use pdt_nondefault_base_m, only: base_t
    implicit none
    type(base_t(4)) :: parent_four
    type(base_t(8)) :: parent_eight
    type(first_child(4)) :: first_four
    type(first_child(8)) :: first_eight
    type(second_child(8)) :: second_eight

    if (parent_four%k /= 4 .or. parent_eight%k /= 8) error stop 1
    if (first_four%k /= 4 .or. first_eight%k /= 8) error stop 2
    if (second_eight%k /= 8) error stop 3
    if (first_eight%parent_default /= 17 .or. &
        second_eight%parent_default /= 17) error stop 4
    if (first_eight%child_default /= 42 .or. &
        second_eight%child_default /= 73) error stop 5
    if (.not. extends_type_of(first_eight, parent_eight) .or. &
        .not. extends_type_of(second_eight, parent_eight)) error stop 6
    if (extends_type_of(first_eight, parent_four)) error stop 7

    call reset_first(first_eight)
    call reset_second(second_eight)
end program
