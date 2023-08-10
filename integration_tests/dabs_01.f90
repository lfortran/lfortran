subroutine e1z(z)
   implicit complex*16 (c,z)
   implicit double precision (a,d-h,o-y)
   if (abs(dabs(dimag(z))) > 1d-10) error stop
   if (abs(dabs(0.0d0)) > 1d-10) error stop
   print *, "dimag(z) =", dimag(z)
   print *, "dabs(dimag(z)) =", dabs(dimag(z))
   print *, "dabs(0.0) = ", dabs(0.0d0)
   return
end

program dabs_01
   complex*16 z
   z = (-1.0d0, -0.0d0)
   call e1z(z)
end program

