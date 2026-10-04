program implicit_interface_44
  double precision diff, x, y, z
  external diff
  x = 3.0d0
  y = 1.0d0
  z = diff(x, y)
  if (z /= 2.0d0) error stop
end program implicit_interface_44

double precision function diff(x, y)
  double precision x, y
  diff = x - y
end function diff
