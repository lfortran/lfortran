submodule(mod_separate_compilation_17) submod_separate_compilation_17

    implicit none

contains

    module function int8_fnv_1( key ) result(hash_code)
        integer(1), intent(inout) :: key(:)
        integer(4)             :: hash_code
        integer(1), parameter :: array(4) = [1, 2, 3, 4]
        integer(4), parameter :: result = 5
        hash_code = result
        key = array
    end function int8_fnv_1

end submodule submod_separate_compilation_17