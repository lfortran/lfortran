program file_50
  implicit none

  integer, parameter :: unit_no = 24
  character(*), parameter :: fn = 'file_50_test.bin'
  integer :: ioerr, iorl

  integer :: i, i1
  logical :: l, pf
  character(5) :: s
  real :: r
  complex :: c
  complex, parameter :: cval = (12.34, 56.78)

  integer, parameter :: wr_order(10) =  &
    [ 5, 3, 6, 2, 8, 1, 9, 7, 10, 4 ]

  open (unit_no, file=fn, status='old', iostat=ioerr)
  if (ioerr == 0) then
    close (unit_no, status='delete')
  end if

  inquire (iolength=iorl) l, s, i, r, c
  if (iorl /= 25) error stop

  open (unit_no, file=fn, access='direct', recl=iorl)

  do i=1, 10
    i1 = wr_order(i)
    l = mod(i1, 2) == 0
    write (unit_no, rec=i1) l, '12345', i1, i1 + 0.5, cval
  end do

  do i=1, 10
    l = .false.; s = 'uini'; i1 = -42; r = -42.42
    read (unit_no, rec=i) l, s, i1, r, c
    pf = (l .eqv. mod(i, 2) == 0) .and. &
         (s == '12345') .and.  &
         (i == i1) .and.  &
         (abs(r - (i + 0.5)) < 0.0001) .and.  &
         (c == cval)
    if (.not. pf) error stop
  end do

  close (unit_no, status='delete')

end program file_50
