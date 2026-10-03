program select_case_02
  implicit none
  character(5) :: s
  integer :: i, j

  ! Test select case on a substring with variable indices
  s = "true "
  i = 1
  j = 4
  select case(s(i:j))
    case("true")
      print *, "matched true"
    case("false")
      print *, "matched false"
    case default
      error stop "expected true"
  end select

  s = "false"
  i = 1
  j = 5
  select case(s(i:j))
    case("true")
      error stop "expected false"
    case("false")
      print *, "matched false"
    case default
      error stop "expected false"
  end select

  ! Test with constant indices (regression check)
  s = "true "
  select case(s(1:4))
    case("true")
      print *, "matched const true"
    case default
      error stop "expected const true"
  end select

  print *, "ok"
end program
