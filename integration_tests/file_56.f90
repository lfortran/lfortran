program read_fmt9
  implicit none

  real :: r1, r2, r3
  double precision :: d1, d2, d3, d4

  open (10, file='file_56_fort.10', status='replace', form='formatted')

! Test E format
  write (10,'(a13)') '+0.339567E+02'
  write (10,'(a13)') '  + .339567+2' ! Not read correctly
  write (10,'(a13)') ' + 3.395670E1'

  rewind (10)
  r1 = 0.0; r2 = 0.0; r3 = 0.0
  read (10, '(e13.6)') r1, r2, r3
  write (*, '(a,e17.6,a)') 'r1 =', r1
      if(abs (r1-33.9567) >= 0.01) error stop
  write (*, '(a,e17.6,a)') 'r2 =', r2
      if(abs (r2-33.9567) >= 0.01) error stop
  write (*, '(a,e17.6,a)') 'r3 =', r3
      if(abs (r3-33.9567) >= 0.01) error stop

! Test F format
  rewind (10)
  write (10,'(a18)') ' 0.96295134244D+04', '   .96295134244D04'
  write (10,'(a18)') '   0.96295134244+4' ! Not read correctly
  write (10,'(a18)') '   +.96295134244D4'

  rewind (10)
  d1 = 0.0; d2 = 0.0; d3 = 0.0
  read (10, '(f18.11)') d1, d2, d3, d4
  write (*, '(a,f18.11,a)') 'd1 =', d1
      if(abs (d1-9629.5134244d0) >= 0.01) error stop
  write (*, '(a,f18.11,a)') 'd2 =', d2
      if(abs (d2-9629.5134244d0) >= 0.01) error stop
  write (*, '(a,f18.11,a)') 'd3 =', d3
      if(abs (d3-9629.5134244d0) >= 0.01) error stop
  write (*, '(a,f18.11,a)') 'd4 =', d4
      if(abs (d4-9629.5134244d0) >= 0.01) error stop

end program
