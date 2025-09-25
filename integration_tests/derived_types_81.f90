module hashmap_wrappers_derived_types_81
    implicit none

    type :: key_type
        integer(4) :: value
    end type key_type

    abstract interface
        pure function hasher_fun( key )  result(hash_value)
            import key_type
            type(key_type), intent(in)    :: key
            integer(4)             :: hash_value
        end function hasher_fun
    end interface

contains

    pure function fnv_1_hasher( key )
        type(key_type), intent(in)    :: key
        integer(4)             :: fnv_1_hasher
        fnv_1_hasher = key % value + 1
    end function fnv_1_hasher
end module

module hashmaps_derived_types_81
    use hashmap_wrappers_derived_types_81
    implicit none

    type :: hashmap_type
        procedure(hasher_fun), pointer, nopass :: hasher => fnv_1_hasher
    end type hashmap_type
end module

program derived_types_81
    use hashmaps_derived_types_81
    implicit none

    type(hashmap_type) :: hmap
    type(key_type) :: key
    integer(4) :: result

    key % value = 1
    result = hmap % hasher(key)

    print *, result
    if (result /= 2) error stop
end program