program io_direct_slash
  implicit none

  integer :: idata1(0:9), idata2(0:9)
  integer :: i, j
  integer :: iolen
  character(40) :: string40

  inquire (iolength=iolen) string40
  if (iolen /= 40) error stop

  open (10, file='io_direct_slash.dat', status='replace', &
      access='direct', form='formatted', recl=iolen)

  do i=1, 10, 2
    write (10, fmt='(10i4/10i4)', rec=i) &
        (j, j=i*10, i*10+9), (j, j=(i+1)*10, (i+1)*10+9)
  end do

  do i=9, 1, -2
    read (10, fmt='(10i4/10i4)', rec=i) idata1, idata2
    ! Verify idata1 contains i*10 .. i*10+9
    do j=0, 9
      if (idata1(j) /= i*10 + j) error stop
    end do
    ! Verify idata2 contains (i+1)*10 .. (i+1)*10+9
    do j=0, 9
      if (idata2(j) /= (i+1)*10 + j) error stop
    end do
  end do

  close (10, status='delete')
  print *, "PASS"

end program
