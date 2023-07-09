subroutine dvode ()
   double precision tau
   common /dvod01/ tau(13)
end

subroutine dvstep ()
   double precision tau
   double precision h
   common /dvod01/ tau(13)

   h = 5.6d0
   tau(1) = h
   return
end

program common_12
   double precision tau
   common /dvod01/ tau(13)

   call dvstep()
   if (abs(tau(1) - 5.6d0) > 1e-10) error stop
   print *, tau(1)
end program


