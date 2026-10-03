module hashmaps_pp
    implicit none

    type :: key_type
        integer(1), allocatable :: value(:)
    end type key_type

    abstract interface
        pure function hasher_fun_temp(key) result(hash_value)
            import key_type
            type(key_type), intent(in) :: key
            integer(4) :: hash_value
        end function hasher_fun_temp
    end interface

    type, abstract :: hashmap_type
        procedure(hasher_fun_temp), pointer, nopass :: hasher
    end type hashmap_type

end module hashmaps_pp

module hashmap_open_pp
    use hashmaps_pp
    implicit none
contains

    subroutine in_open_map(map, key)
        class(hashmap_type), intent(inout) :: map
        type(key_type), intent(in) :: key
        integer(4) :: hash_val
        hash_val = map%hasher(key)
    end subroutine in_open_map

end module hashmap_open_pp

program procedure_pointer_abstract_01
    implicit none
    print *, "Compilation test passed"
end program procedure_pointer_abstract_01
