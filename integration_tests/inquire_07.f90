program inquire3
  implicit none

  integer :: i, iolen
  logical :: l
  character(4) :: s
  real :: r

  inquire (iolength=iolen) l, s, i, r
  print *, 'iolength =', iolen

end program