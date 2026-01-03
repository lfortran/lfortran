module stdlib_hashmaps_derived_types_61

    implicit none

    type :: open_map_entry_type
        integer  :: slot = 0
    end type open_map_entry_type

    type open_map_entry_ptr
        type(open_map_entry_type), pointer :: target => null()
    end type open_map_entry_ptr


    type :: open_hashmap_type
        type(open_map_entry_ptr), allocatable  :: inverse(:)
    end type open_hashmap_type

end module stdlib_hashmaps_derived_types_61

program derived_types_61

    use stdlib_hashmaps_derived_types_61

    implicit none

    type(open_hashmap_type)   :: map
    integer(4) :: inmap = 1
    
    allocate(map % inverse(1))
    allocate(map % inverse(1) % target)

    print *, map % inverse(inmap) % target % slot
    if (map % inverse(inmap) % target % slot /= 0) error stop
    
end program