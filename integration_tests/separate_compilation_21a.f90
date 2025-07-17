module mod_separate_compilation_21

    implicit none

    type :: key_type
        integer(1), allocatable :: value(:)
    end type key_type

    interface fnv_1_hash_sc_21
        module function int8_fnv_1_sc_21( key ) result(hash_code)
            integer(1), intent(inout) :: key(:)
            integer(4)             :: hash_code
        end function int8_fnv_1_sc_21
    end interface fnv_1_hash_sc_21

end module mod_separate_compilation_21