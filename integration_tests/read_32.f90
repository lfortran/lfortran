program daio3
  implicit none

  integer, parameter :: unit_no = 24
  integer :: i, i1

  open (unit_no, file='fort.24', access='direct', recl=132)

  do i = 1, 10
    write (unit_no, rec=i) i
  end do

  do i = 1, 10
    read (unit_no, rec=i) i1
    if (i1 /= i) error stop
  end do

  close (unit_no)

  print *, "all tests passed"

end program