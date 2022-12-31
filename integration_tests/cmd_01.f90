program test_get_command_argument
implicit none
    integer :: i, count
    character(len=32) :: arg

    count = command_argument_count()

    do i = 0, count
        call get_command_argument(i, arg)
        print *, trim(arg)
    end do

end program
