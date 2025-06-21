module class_54_m
    implicit none

    type :: hashmap_type
        integer(4) :: num_entries = 0
    end type hashmap_type
end module class_54_m


program test_open_maps
    use class_54_m
    implicit none

    type(hashmap_type)   :: map

    call init_open_map(map)
    map%num_entries = map%num_entries + 1
    call init_open_map(map)

    if (map%num_entries /= 0) error stop

contains

    subroutine init_open_map( map )
        class(hashmap_type), intent(out)      :: map
    end subroutine init_open_map
end program test_open_maps
