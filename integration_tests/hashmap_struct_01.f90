module stdlib_hashmaps_hashmap_struct_01
    implicit none
    type :: open_hashmap_type
        integer(4) :: slots
    end type open_hashmap_type
end module stdlib_hashmaps_hashmap_struct_01

program hashmap_struct_01
    use stdlib_hashmaps_hashmap_struct_01
    implicit none
    type(open_hashmap_type) :: map
    call in_open_map(map)

contains

    subroutine expand_open_slots(map)
        type(open_hashmap_type), intent(inout) :: map
    end subroutine expand_open_slots

    subroutine in_open_map(map)
        class(open_hashmap_type), intent(inout) :: map
        call expand_open_slots(map)
    end subroutine in_open_map

end program
