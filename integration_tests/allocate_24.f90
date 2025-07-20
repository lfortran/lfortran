program allocate_24
  integer,allocatable:: x
  x = 666
  print "(I0)", x
  if (x /= 666) error stop
  deallocate(x)
end program
