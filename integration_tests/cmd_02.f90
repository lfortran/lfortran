program test_exec

    character(len=255) :: homedir
    call get_environment_variable("HOME", homedir)
    print *, trim(homedir)

    call execute_command_line("printenv")

end program test_exec
