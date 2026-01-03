      program main
      integer k
      k = 1
100   print *, "hello"
200   k = k+1

      integer m,n
      m = 1
      n = 2
      goto (200, 100) m
      goto (100, 200) n
      end program
