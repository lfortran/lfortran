program read_98
  implicit none

  character(len=12) :: file(1)
  integer :: year(1)

  file = "202612231200"
  read (file(1:1)(1:4), '(i4)') year

  if (year(1) /= 2026) error stop "incorrect year"
end program read_98
