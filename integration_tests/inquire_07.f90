program inquire_07
  implicit none

  integer :: i, iolen
  logical :: l
  character(4) :: s
  real :: r

  inquire (iolength=iolen) l, s, i, r
  
  if (iolen /= 16) then
     error stop "inquire(iolength=...) produced wrong result"
  end if
end program inquire_07