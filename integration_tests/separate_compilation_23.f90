program separate_compilation_23
    use mod_separate_compilation_23, only : open_hashmap_type

    implicit none

    type(open_hashmap_type)   :: map
    integer :: key = 1
    call map % map_entry_sc_23( key )

    print *, key
    if (key /= 5) error stop
end program