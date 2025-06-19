program intrinsics_388
  integer exit_status
  exit_status = -1

  call execute_command_line( &
    command = "export TEST_ENV_VAR_VALUE=23", &
    wait = .true., &
    exitstat = exit_status &
  )

  print *, "Exit status after setting environment variable:", exit_status
  if (exit_status /= 0) error stop

end program intrinsics_388

