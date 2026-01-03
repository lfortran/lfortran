module mod_submodule_05
    implicit none

    interface
        module subroutine map_open_entry(key)
            integer, intent(inout) :: key
        end subroutine map_open_entry
    end interface
    
end module mod_submodule_05

submodule(mod_submodule_05) submod_submodule_05
    implicit none
contains

    module subroutine map_open_entry(key)
        integer, intent(inout) :: key
        integer, parameter :: i = 1
        key = i
    end subroutine map_open_entry

end submodule submod_submodule_05

program submodule_05
    use mod_submodule_05

    implicit none

    integer :: key = 5
    call map_open_entry( key )

    print *, key
    if (key /= 1) error stop
end program