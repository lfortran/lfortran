program exit_04
  implicit none
  integer :: n
  n = 0

  s: select case (n)
  case (0)
    exit s
    n = 5
  end select s

  a: associate (m => n)
    exit a
    n = 2
  end associate a

  my_if: if (n == 0) then
    exit my_if
  end if my_if

  if (n /= 0) error stop
  print *, "ok"
end program
