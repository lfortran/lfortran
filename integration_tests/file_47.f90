program daio_test
  implicit none

! Simple direct access I/O test with LOGICAL arrays

  integer :: daunit, dall
  integer :: i
  logical :: l(10), lanswer

  dall = -42
  inquire (iolength=dall) l
  print *, 'logical record iolength =', dall

  open (newunit=daunit, file='xyzzy.dat', access='direct', form='unformatted', recl=dall)

  l = .true.
  do, i=1, 5
    l = .not. l
    write (daunit, rec=i) l
  end do

  lanswer = .true.
  do, i=5, 1, -1
    lanswer = .not. lanswer
    read (daunit, rec=i) l
    print *, 'record', i
    if (any(l .neqv. lanswer)) error stop
  end do

  close (daunit, status='delete')

end program