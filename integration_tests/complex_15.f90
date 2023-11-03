program complex_15
   implicit none
   complex(8) :: k, z
   k = (3, -4)
   z = -k
   print *, k, z

   if (abs(z) == k) error stop
end program
