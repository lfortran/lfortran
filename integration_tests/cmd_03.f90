! Test execute_command_line with --std=f23 (issue #4676)
program cmd_03
    implicit none
    call execute_command_line('echo passed')
end program cmd_03
