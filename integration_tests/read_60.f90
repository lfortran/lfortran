program read_60
  implicit none

  character(len=10) :: s
  character(5), allocatable :: a(:)
  integer :: ios

  allocate(a(1))

  s = "hello"

  read(s, *, iostat=ios) a
  if (ios /= 0) then
     error stop "READ failed"
  end if

  if (a(1) /= "hello") then
     error stop "Wrong value in a(1)"
  end if

  print *, "PASS"

end program read_60