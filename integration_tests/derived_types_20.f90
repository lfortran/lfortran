program main
use fpm_command_line_20, only: fpm_cmd_settings, get_command_line_settings

implicit none

class(fpm_cmd_settings), allocatable :: cmd_settings

call get_command_line_settings(cmd_settings)

end program main
