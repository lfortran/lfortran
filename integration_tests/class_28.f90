module stdlib_hashmap_wrappers_class_28

    implicit none
    private

    public :: other_type

    type :: other_type
        class(*), allocatable :: value
    end type other_type

end module stdlib_hashmap_wrappers_class_28


module stdlib_hashmaps_class_28

    use stdlib_hashmap_wrappers_class_28

    implicit none

    type :: open_map_entry_type
        type(other_type)  :: other
    end type open_map_entry_type

end module stdlib_hashmaps_class_28


program class_28

    use stdlib_hashmaps_class_28
    use stdlib_hashmap_wrappers_class_28

    implicit none

    type(open_map_entry_type), pointer :: new_ent
    type(other_type) :: other
    allocate(new_ent)

    other = new_ent % other

end program