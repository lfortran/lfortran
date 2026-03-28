program select_case_01
  implicit none
  integer :: n

  ! Test that select case evaluates its expression exactly once
  n = 0
  select case (f())
    case (1)
      print *, "case 1"
    case (2)
      print *, "case 2"
  end select
  if (n /= 1) error stop "FAIL: select case evaluated expression multiple times"

  ! Test with a different return value
  n = 0
  select case (g())
    case (10)
      print *, "case 10"
    case (20:30)
      print *, "case 20-30"
    case default
      print *, "default"
  end select
  if (n /= 1) error stop "FAIL: select case evaluated expression multiple times (range)"

  print *, "OK"
contains
  function f() result(r)
    integer :: r
    n = n + 1
    r = 2
  end function

  function g() result(r)
    integer :: r
    n = n + 1
    r = 25
  end function
end program
