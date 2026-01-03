module mod_separate_compilation_16
    implicit none

    interface
        module subroutine map_open_entry(key)
            integer, intent(inout) :: key
        end subroutine map_open_entry
    end interface
    
end module mod_separate_compilation_16