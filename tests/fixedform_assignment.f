      integer a, b,c,d,            k
      a = 1
      b = 2
      c = a *b/2
      d = c*c
      print *, a, b, c, d
      do 80 k=1,20
        print *, 1
80     end do
 
120      if  (a < b) then
        a = 2 * b
      else
        b = 33 * c
      endif
      end

