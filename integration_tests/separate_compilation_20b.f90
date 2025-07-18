submodule(mod_separate_compilation_20) submod_separate_compilation_20
    implicit none
contains

    module subroutine map_open_entry_sc_20(key)
        integer, intent(inout) :: key
        integer, parameter :: i = 1
        key = i
    end subroutine map_open_entry_sc_20

end submodule submod_separate_compilation_20