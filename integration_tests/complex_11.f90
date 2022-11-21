program complex_11
   implicit none
   complex(8) :: k
   real::re,im
   re=3
   im=4
   k = cmplx(re, im)
   print *, k

end program
