program read_list6
  implicit none

! Test various null inputs

  integer :: i1, i2, i3, i4, i5, i6, i7
  real :: r1, r2
  logical :: l1, l2
  character(4) :: s1, s2

  integer, parameter :: lun_io = 42

  open (lun_io, status='scratch', form='formatted')
  write (lun_io, '(a)') ",4, 1*, 8, ,, 14"
  write (lun_io, '(a)') "5, -0.25E1, 4*, 'TEST', F"

  rewind (lun_io)

  print *, 'test 1:'
  i1 = -1; i2 = -2; i3 = -3; i4 = -4; i5 = -5; i6 = -6; i7 = -7
  read (lun_io, *) i1, i2, i3, i4, i5, i6, i7
  print *, 'i1 =', i1, pf (i1 == -1)
  print *, 'i2 =', i2, pf (i2 ==  4)
  print *, 'i3 =', i3, pf (i3 == -3)
  print *, 'i4 =', i4, pf (i4 ==  8)
  print *, 'i5 =', i5, pf (i5 == -5)
  print *, 'i6 =', i6, pf (i6 == -6)
  print *, 'i7 =', i7, pf (i7 == 14)

  print *
  print *, 'test 2:'
  l1 = .false.; l2 = .true.
  r1 = -42.0; r2 = -43.0
  s1 = 'abcd'; s2 = 'bcde'
  read (lun_io, *) i1, r1, i2, l1, r2, s1, s2, l2
  print *, 'i1 =', i1, pf (i1 == 5)
  print *, 'r1 =', r1, pf (abs (r1 - (-0.25e1)) < 0.0001)
  print *, 'i2 =', i2, pf (i2 == 4)
  print *, 'l1 =', l1, pf (.not. l1)
  print *, 'r2 =', r2, pf (abs (r2 - (-43.0)) < 0.0001)
  print *, 's1 = ', s1, pf (s1 == 'abcd')
  print *, 's2 = ', s2, pf (s2 == 'TEST')
  print *, 'l2 =', l2, pf (.not. l2)

contains

  character(6) function pf (l)
    logical, intent(in) :: l
    pf = merge (': pass', ': fail', l)
  end function

end program