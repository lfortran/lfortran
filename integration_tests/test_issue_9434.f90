program test_inquire_full
  implicit none
  integer :: u, num
  logical :: is_opened
  character(len=20) :: acc
  character(len=20) :: fname = "test_inquire_full.txt"

  open(newunit=u, file=fname, access="SEQUENTIAL", status="unknown")
  
  ! Inquire by FILENAME, but ask for NUMBER and ACCESS too
  inquire(file=fname, opened=is_opened, number=num, access=acc)
  
  print *, "Opened:", is_opened
  print *, "Unit:  ", num
  print *, "Access:", trim(acc)

  close(u, status="delete")
end program
