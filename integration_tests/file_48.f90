program daio4
  implicit none

  integer, parameter :: unit_no = 24
  integer :: i, i1, iolen
  logical :: l, pf
  character(4) :: s
  real :: r
  complex :: c
  complex, parameter :: cval = (12.34, 56.78)

!  inquire (iolength=iolen) l, s, i, r, c  ! Causes ICE: see PR #9761
!
! So I hard code iolen. 
  iolen = 24
  open (unit_no, file='fort.24', access='direct', recl=iolen)

  do, i=1, 10
    l = mod (i,2) == 0
    write (unit_no, rec=i) l, 'wxyz', i, i+0.5, cval
  end do

! Read fails to read correctly:
  do, i=1, 10
    l = .false.; s = 'uini'; i1 = -42; r = -42.42
    read (unit_no, rec=i) l, s, i1, r, c
    pf = (l .eqv. mod (i, 2) == 0) .and. &
         (s == 'wxyz') .and.  &
         (i == i1) .and.  &
         (abs (r - (i + 0.5)) < 0.0001) .and.  &
         (c == cval)
    print *, 'l, s, i1, r =', l, s, i1, r, c
    print *, 'read', i, merge ('pass', 'fail', pf)
    if (.not. pf) error stop
  end do

  close (unit_no)

end program