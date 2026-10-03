! Test: substring with end < start yields zero-length string (F2018 9.4.1)
program string_107
  implicit none
  character(len=5) :: s = "hello"
  character(len=:), allocatable :: res
  integer :: j

  ! s(1:-1) should be zero-length
  j = 0
  res = trim(s(1:j-1))
  if (len(res) /= 0) error stop

  ! s(3:2) should be zero-length
  res = s(3:2)
  if (len(res) /= 0) error stop

  ! s(5:1) should be zero-length
  res = s(5:1)
  if (len(res) /= 0) error stop

  ! Normal substring still works
  res = s(2:4)
  if (len(res) /= 3) error stop
  if (res /= "ell") error stop

  ! s(1:1) single character
  res = s(1:1)
  if (len(res) /= 1) error stop
  if (res /= "h") error stop

  print *, "ok"
end program string_107
