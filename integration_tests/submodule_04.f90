module mod_submodule_04
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
    
end module mod_submodule_04

submodule(mod_submodule_04) submod_submodule_04
    implicit none
contains

    module subroutine map_open_entry(map, key)
        class(open_hashmap_type), intent(inout) :: map
        integer, intent(inout) :: key
        integer, parameter :: i = 1
        key = i
    end subroutine map_open_entry

end submodule submod_submodule_04

program submodule_04
    use mod_submodule_04, only : open_hashmap_type

    implicit none

    type(open_hashmap_type)   :: map
    integer :: key = 3
    call map % map_entry( key )

    print *, key
    if (key /= 1) error stop
end program