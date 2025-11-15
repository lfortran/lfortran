program intrinsics_394
    implicit none
    character(len=:), allocatable :: dir, cmd
    integer :: stat

    call check_dir_exists("fortran_scratch")
    call check_dir_exists("fortran_scratch2")

contains

    subroutine check_dir_exists(dirname)
        character(len=*), intent(in) :: dirname
        character(len=:), allocatable :: local_cmd
        integer :: exitstat

        ! Build the test command
        local_cmd = "test -d " // trim(dirname)

        ! Execute and capture exit status
        call execute_command_line(local_cmd, exitstat=exitstat)

        ! Print robust diagnostic info
        if (exitstat == 0) then
            print *, "Directory exists: ", trim(dirname)
        else
            print *, "Directory does NOT exist: ", trim(dirname)
        end if

    end subroutine check_dir_exists

end program intrinsics_394
