module stdlib_hashmaps_hashmap_nested_dealloc_derived_pointer

    implicit none
    
    type :: open_map_entry_type
        integer :: slots
    end type open_map_entry_type

    type :: open_map_entry_pool
        type(open_map_entry_type), allocatable :: more_map_entries(:)
    end type open_map_entry_pool

    type :: open_hashmap_type
        type(open_map_entry_pool), pointer :: cache => null()
    end type open_hashmap_type

end module stdlib_hashmaps_hashmap_nested_dealloc_derived_pointer


program hashmap_nested_dealloc_derived_pointer
    use stdlib_hashmaps_hashmap_nested_dealloc_derived_pointer

    implicit none

    type(open_hashmap_type)   :: map
    type(open_map_entry_pool), pointer :: map_entry_pool_head

    allocate(map_entry_pool_head)
    allocate(map_entry_pool_head % more_map_entries(10))

    do while(associated(map % cache))
        deallocate( map_entry_pool_head % more_map_entries )
    end do

end program hashmap_nested_dealloc_derived_pointer