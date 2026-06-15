program select_case_04
  implicit none
  logical :: b

  b = .true.
  select case (b)
  case (.true.)
    print *, "true branch"
  case (.false.)
    error stop "expected true branch"
  end select

  b = .false.
  select case (b)
  case (.true.)
    error stop "expected false branch"
  case (.false.)
    print *, "false branch"
  end select

  b = .true.
  select case (b)
  case (.true.)
    print *, "default not taken"
  case default
    error stop "expected true branch, not default"
  end select

  print *, "ok"
end program
