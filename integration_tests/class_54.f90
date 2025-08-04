module class_54_m
    implicit none

    type :: hashmap_type
        integer(4) :: num_entries = 3
    end type hashmap_type
end module class_54_m


program test_open_maps
    use class_54_m
    implicit none

    type(hashmap_type)   :: map, map2

    ! intent(out)
    print *, map%num_entries
    if (map%num_entries /= 3) error stop
    call init_open_map(map)
    print *, map%num_entries
    if (map%num_entries /= 3) error stop
    map%num_entries = map%num_entries + 1
    print *, map%num_entries
    if (map%num_entries /= 4) error stop
    call init_open_map(map)
    print *, map%num_entries
    if (map%num_entries /= 3) error stop

    ! intent(inout)
    print *, map2%num_entries
    if (map2%num_entries /= 3) error stop
    call init_open_map_inout(map2, 3)
    print *, map2%num_entries
    if (map2%num_entries /= 3) error stop
    map2%num_entries = map2%num_entries + 1
    print *, map2%num_entries
    if (map2%num_entries /= 4) error stop
    call init_open_map_inout(map2, 4)
    print *, map2%num_entries
    if (map2%num_entries /= 4) error stop

contains

    subroutine init_open_map( map )
        class(hashmap_type), intent(out)      :: map
        print *, map%num_entries
        if (map%num_entries /= 3) error stop
    end subroutine init_open_map

    subroutine init_open_map_inout( map, i )
        class(hashmap_type), intent(inout)      :: map
        integer, intent(in) :: i
        print *, i, map%num_entries
        if (map%num_entries /= i) error stop
    end subroutine init_open_map_inout

end program test_open_maps
