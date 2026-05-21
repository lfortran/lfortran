program read_73
  implicit none
  character :: s160
  character(2) :: s230
  common s160(60), s230(30)
  character(*), parameter :: s1 = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ =+-*/(),.*0*1.2,3)4(5/6'
  character(*), parameter :: s2 = '6/5(4)3,2.1*0*.,)(/*-+= ZYXWVUTSRQPONMLKJIHGFEDCBA9876543210'

  logical :: pf

100 format (60A1)
101 format (30a2)

  open (42, file='read_73.dat', status='replace', form='formatted')
  write (42, '(a)') s1
  write (42, '(a)') s2

  rewind (42)

  s160 = '?'
  read (42,100) s160
  pf = all (s160 == transfer (s1, mold=s160))
  if (.not. pf) then
    print *, 's160 test failed'
    error stop
  end if
  s230 = '??'
  read (42,101) s230
  pf = all (s230 == transfer (s2, mold=s230))
  if (.not. pf) then
    print *, 's230 test failed'
    error stop
  end if

  close (42)

end program read_73