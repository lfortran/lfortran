program intrinsics_387
    implicit none
    character(len=:), allocatable :: temp

    temp = "echo Hello from Fortran!"
    call execute_command_line(temp)

    temp = "export LFORTRAN_TEST_VAR_INTRINSIC=42; echo $LFORTRAN_TEST_VAR_INTRINSIC"
    call execute_command_line(temp)
end program intrinsics_387

