program intrinsics_317
    implicit none
    integer :: arg_num
    character(len=100) :: arg_value
    integer :: arg_length, arg_status

    ! Print a message if no arguments are passed
    call get_command_argument(0, arg_value)
    if (trim(arg_value) == "") then
        print *, "No command-line arguments provided."
    else
        ! Loop through the arguments passed on the command line
        do arg_num = 1, 3  ! Assuming we want to read 3 arguments
            call get_command_argument(arg_num, arg_value, arg_length, arg_status)
            if (trim(arg_value) /= "") then
                print *, "Argument ", arg_num, ": ", trim(arg_value)
                print *, "Length", arg_length
                print *, "Status", arg_status
            else
                print *, "No more arguments."
                exit
            end if
        end do
    end if
end program
