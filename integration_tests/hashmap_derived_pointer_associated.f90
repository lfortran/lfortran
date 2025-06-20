module stdlib_hashmaps_hashmap_derived_pointer_associated

    implicit none

    integer(4), parameter :: pool_size = 64

    type :: open_map_entry_type
        integer(4) :: hash_val
    end type open_map_entry_type

    type :: open_map_entry_pool
        integer(4)                     :: next = 0
        type(open_map_entry_type), allocatable :: more_map_entries(:)
        type(open_map_entry_pool), pointer     :: lastpool => null()
    end type open_map_entry_pool

    type :: open_hashmap_type
        type(open_map_entry_pool), pointer    :: cache => null()
    end type open_hashmap_type

end module stdlib_hashmaps_hashmap_derived_pointer_associated


module stdlib_hashmap_open_hashmap_derived_pointer_associated

    use stdlib_hashmaps_hashmap_derived_pointer_associated
    implicit none

contains

    subroutine extend_open_map_entry_pool(pool) 
        type(open_map_entry_pool), intent(inout), pointer :: pool
        type(open_map_entry_pool), pointer :: map_entry_pool_head

        allocate(map_entry_pool_head)

        allocate(map_entry_pool_head % more_map_entries(0:pool_size-1))
        map_entry_pool_head % lastpool => pool
        pool => map_entry_pool_head
        pool % next = 0

    end subroutine extend_open_map_entry_pool


    module subroutine init_open_map( map )
        class(open_hashmap_type), intent(out)      :: map

        type(open_map_entry_pool), pointer :: pool => null()
        call extend_open_map_entry_pool(map % cache)

        if (associated(map % cache)) then
            print *, "Pointer is Associated"
        else
            print *, "Pointer is not Associated"
            error stop "Pointer is not associated"
        end if

        pool => map % cache
        print *, pool % next

    end subroutine init_open_map


end module stdlib_hashmap_open_hashmap_derived_pointer_associated


program hashmap_derived_pointer_associated

    use stdlib_hashmaps_hashmap_derived_pointer_associated, only : open_hashmap_type
    use stdlib_hashmap_open_hashmap_derived_pointer_associated

    implicit none

    type(open_hashmap_type) :: map

    call init_open_map(map)

end program hashmap_derived_pointer_associated