module mod_separate_compilation_17

    implicit none

    type :: key_type
        integer(1), allocatable :: value(:)
    end type key_type

    interface fnv_1_hash
        module function int8_fnv_1( key ) result(hash_code)
            integer(1), intent(inout) :: key(:)
            integer(4)             :: hash_code
        end function int8_fnv_1
    end interface fnv_1_hash

end module mod_separate_compilation_17