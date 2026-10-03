program test

   integer, allocatable :: nz(:)
   integer ::  maxnz(1), mx

   nz = [480,120,1]

   where (nz > 1)
      maxnz = maxval(nz)
   end where
   mx = maxnz(1)

   print *, mx

end program test
