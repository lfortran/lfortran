program class_allocate_03
    implicit none

    type :: base
    end type base

    type, extends(base) :: derived
        character(len=20), allocatable :: names(:)
    end type derived

    class(base), allocatable :: obj

    call get_command_line_settings(obj)

    select type(obj)
        type is(derived)
            if (size(obj%names) /= 2) error stop
            if (obj%names(1) /= 'global_name1') error stop
            if (obj%names(2) /= 'global_name2') error stop
    end select

contains

     subroutine get_command_line_settings(cmd_settings)
        class(base), allocatable, intent(out) :: cmd_settings
        character(len=20), allocatable :: global_names(:)
        allocate(character(len=20) :: global_names(2))
        global_names = ['global_name1', 'global_name2']
        allocate(derived :: cmd_settings)
        cmd_settings = derived(names = global_names)
    end subroutine get_command_line_settings

end program class_allocate_03
