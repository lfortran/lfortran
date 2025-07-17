submodule(mod_separate_compilation_23) submod_separate_compilation_23
    implicit none
contains

    module subroutine map_open_entry_sc_23(map, key)
        class(open_hashmap_type), intent(inout) :: map
        integer, intent(inout) :: key
        integer, parameter :: i = 5
        key = i
    end subroutine map_open_entry_sc_23

end submodule submod_separate_compilation_23