module mod_separate_compilation_19
    implicit none
    private

    public ::  open_hashmap_type

    type :: open_hashmap_type
    contains
        procedure :: map_entry => map_open_entry
    end type open_hashmap_type

    interface
        module subroutine map_open_entry(map, key)
            class(open_hashmap_type), intent(inout) :: map
            integer, intent(inout) :: key
        end subroutine map_open_entry
    end interface
    
end module mod_separate_compilation_19