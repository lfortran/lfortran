      program p
      integer i, s
      s = 0
      do 1 i=1,10
       if (i .eq. 3) go to 1
       s = s + i
1     continue
      if (s .ne. 52) error stop
      end program
