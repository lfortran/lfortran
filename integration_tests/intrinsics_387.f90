program intrinsics_387
    implicit none
    character(len=:), allocatable :: temp
    integer :: stat = -1

    temp = "echo Hello from Fortran!"
    call execute_command_line(temp)

    temp = "export LFORTRAN_TEST_VAR_INTRINSIC=42; echo $LFORTRAN_TEST_VAR_INTRINSIC"
    call execute_command_line(temp)

    call execute_command_line(temp, exitstat=stat)
    print *, "Exit status:", stat
    if ( stat /= 0 ) error stop 

end program intrinsics_387

