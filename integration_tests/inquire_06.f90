program inquire_recl
  implicit none
  integer :: u, r

  open(newunit=u, file="inquire_06.bin", access="direct", recl=32)
  inquire(unit=u, recl=r)

  if (r /= 32) error stop
  
  close(u, status="delete")
end program
