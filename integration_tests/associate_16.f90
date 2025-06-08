module stdlib_hashmaps_associate_16

    implicit none

    type :: open_map_entry_type
        integer(4) :: hash_val
    end type open_map_entry_type


    type open_map_entry_ptr
        type(open_map_entry_type), pointer :: target => null()
    end type open_map_entry_ptr


    type :: open_hashmap_type
        type(open_map_entry_ptr), allocatable  :: inverse(:)
    end type open_hashmap_type

end module stdlib_hashmaps_associate_16


program associate_16

    use stdlib_hashmaps_associate_16

    implicit none

    type(open_hashmap_type)   :: map
    integer(4) :: inv_index = 1
    integer(4) :: test = 1 

    allocate(map % inverse(1))
    allocate(map % inverse(1) % target)
                                           
    associate(inverse => map % inverse(inv_index))
        if (associated(inverse % target)) then
            test = 2
        end if
    end associate

    print *, test

    if (test /= 2) error stop
end program