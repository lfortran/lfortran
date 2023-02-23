program test_get_command_argument
implicit none
    integer :: i = 0, count
    character(len=32) :: arg

    count = command_argument_count()
    print *, "count: ", count
    call get_command_argument(i, arg)
    print *, trim(arg)

end program
