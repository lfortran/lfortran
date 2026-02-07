program read_fmt_tabs
  implicit none
  character(*), parameter :: infmt = '(I6, T1, I6, TL6, I6, TL9, I6, t1, tr2, i3)'
  integer :: i, ia(5)
  integer :: iounit
  open (newunit=iounit, file='fort.data', form='formatted')
  write (iounit,'(a)') '12345'

  rewind (iounit)
  ia = -42
  read (iounit, infmt) (ia(i), i=1, 5)
  if (.not. all(ia(1:4) == 12345)) error stop
  if (ia(5) /= 345) error stop
  close (iounit)
  print *, "all tests passed"
end program read_fmt_tabs
