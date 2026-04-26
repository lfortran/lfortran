program read_80
  implicit none

  character(10) :: cdata = '12 345 678'
  complex :: c1, c2

  read (cdata, 100) c1,            c2
100 format (bz,    2F5.2, bn, t1, 2F5.1)

  if (abs (real  (c1) - 120.34) > 0.0001) error stop
  if (abs (aimag (c1) - 506.78) > 0.0001) error stop
  if (abs (real  (c2) - 123.4) > 0.0001) error stop
  if (abs (aimag (c2) - 567.8) > 0.0001) error stop

end program read_80
