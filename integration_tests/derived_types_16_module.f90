module fpm_command_line
implicit none
public :: fpm_run_settings, get_command_line_settings

type :: fpm_run_settings
    integer :: int
    real :: float
    logical :: bool
end type

contains

    subroutine get_command_line_settings(cmd_settings)
        class(fpm_run_settings), allocatable, intent(out) :: cmd_settings
        cmd_settings=fpm_run_settings(0, 1.0, .true.)
    end subroutine get_command_line_settings

end module fpm_command_line

program derived_types_16
use fpm_command_line
implicit none
class(fpm_run_settings), allocatable :: settings
allocate(settings)
call get_command_line_settings(settings)
if (settings%int /= 0) error stop
if (abs(settings%float - 1.0) > 1e-6) error stop
if (settings%bool .neqv. .true.) error stop
end program
