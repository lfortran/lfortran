program complex_10
   implicit none
   complex(8) :: k, z
   k = (3, -4)
   z = conjg(k)
   print *, k, z

   if (abs(z - (3,4)) > 1e-10) error stop
end program
