subroutine dzror()
   implicit none
   double precision zxlo
 entry dstzr(zxlo)
   if (abs(zxlo - 1.2) > 1e-7) error stop
   print *, 'dstzr: zxlo = ', zxlo
   return
end

program entry_07
   call dstzr(1.2d0)
end program

