PROGRAM get_environment_variable1
    implicit none
    character(len=255) :: pwd
    character(len=255) :: path
    CALL get_environment_variable("PWD", pwd)
    CALL get_environment_variable("PATH", path)
    print *, pwd
    print *, path
END PROGRAM
