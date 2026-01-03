module stdlib_hashmaps_associate_17

    implicit none

    type :: open_map_entry_type
        integer(4) :: inmap
    end type open_map_entry_type

    type open_map_entry_ptr
        type(open_map_entry_type), pointer :: target => null()
    end type open_map_entry_ptr

    type :: open_hashmap_type
        type(open_map_entry_ptr), allocatable  :: inverse(:)
    end type open_hashmap_type
end module stdlib_hashmaps_associate_17


program associate_17

    use stdlib_hashmaps_associate_17

    implicit none

    type(open_hashmap_type)   :: map
    integer(4) :: inmap = 1
    type(open_map_entry_type), pointer :: temp => null()

    allocate(map % inverse(1))
    allocate(map % inverse(1) % target)

    temp => map % inverse(1) % target
    temp % inmap = 42

    associate( target => map % inverse(inmap) % target )
        print *, target % inmap
        if (target % inmap /= 42) error stop
    end associate

end program
