module fpm_command_line_20
use M_CLI2_20, only : specified
implicit none

type, abstract :: fpm_cmd_settings
    character(len=:), allocatable :: working_dir
    logical :: verbose=.true.
end type

type, extends(fpm_cmd_settings)  :: fpm_new_settings
    character(len=:), allocatable :: name
    logical :: with_executable=.false.
    logical :: with_test=.false.
    logical :: with_lib=.true.
    logical :: with_example=.false.
    logical :: with_full=.false.
    logical :: with_bare=.false.
    logical :: backfill=.true.
end type

contains
    subroutine get_command_line_settings(cmd_settings)
        class(fpm_cmd_settings), allocatable, intent(out) :: cmd_settings

            allocate(fpm_new_settings :: cmd_settings)
            if (any( specified([character(len=10) :: 'src','lib','app','test','example']) ) ) then
                cmd_settings=fpm_new_settings(&
                    & backfill=.false., &
                    & name="xyz", &
                    & with_executable=.true., &
                    & with_lib=.false., &
                    & with_test=.true., &
                    & with_example=.false., &
                    & verbose=.true. )
            end if
    end subroutine get_command_line_settings

end module fpm_command_line_20
