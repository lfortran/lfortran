program test_exec
    integer :: i

    character(len=255) :: homedir
    call get_environment_variable("home", homedir)
    print *, trim(homedir)

    call execute_command_line("printenv")

end program test_exec
