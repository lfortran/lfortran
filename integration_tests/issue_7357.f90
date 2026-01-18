module stdlib_hashmaps
    implicit none

    type open_map_entry_ptr
        integer(4) :: inmap
    end type open_map_entry_ptr

    type :: open_hashmap_type
        type(open_map_entry_ptr), allocatable :: inverse(:)
    end type open_hashmap_type
end module stdlib_hashmaps

program main
    use stdlib_hashmaps
    implicit none

    type(open_hashmap_type) :: map
    type(open_map_entry_ptr), allocatable :: dummy_inverse(:)

    allocate(dummy_inverse(1))
    call move_alloc( dummy_inverse, map % inverse )

    print *, size(map % inverse)

    if (allocated(dummy_inverse)) error stop
    if (.not. allocated(map%inverse)) error stop
    if (size(map%inverse) /= 1) error stop
end program