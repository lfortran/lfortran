submodule(mod_separate_compilation_16) submod_separate_compilation_16
    implicit none
contains

    module subroutine map_open_entry(key)
        integer, intent(inout) :: key
        integer, parameter :: i = 1
        key = i
    end subroutine map_open_entry

end submodule submod_separate_compilation_16