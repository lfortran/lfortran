program separate_compilation_19
    use mod_separate_compilation_19, only : open_hashmap_type

    implicit none

    type(open_hashmap_type)   :: map
    integer :: key = 1
    call map % map_entry( key )

    print *, key
    if (key /= 5) error stop
end program