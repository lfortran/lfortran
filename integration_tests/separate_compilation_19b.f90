submodule(mod_separate_compilation_19) submod_separate_compilation_19
    implicit none
contains

    module subroutine map_open_entry(map, key)
        class(open_hashmap_type), intent(inout) :: map
        integer, intent(inout) :: key
        integer, parameter :: i = 5
        key = i
    end subroutine map_open_entry

end submodule submod_separate_compilation_19