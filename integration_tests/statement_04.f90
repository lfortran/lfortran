program statement
   integer, parameter :: wp = kind(1.e0)
   real(wp) :: g2
   complex(wp) :: g, t
   real(wp) :: ABSSQ
   ABSSQ( t ) = real( t )**2 + aimag( t )**2

   g = (3.0, 4.0)
   g2 = ABSSQ(g)

   print *, g2

end program
