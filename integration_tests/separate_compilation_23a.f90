module mod_separate_compilation_23
    implicit none
    private

    public ::  open_hashmap_type

    type :: open_hashmap_type
    contains
        procedure :: map_entry_sc_23 => map_open_entry_sc_23
    end type open_hashmap_type

    interface
        module subroutine map_open_entry_sc_23(map, key)
            class(open_hashmap_type), intent(inout) :: map
            integer, intent(inout) :: key
        end subroutine map_open_entry_sc_23
    end interface
    
end module mod_separate_compilation_23