subroutine dzror(n, r)
   implicit none
   double precision abstol,reltol,zx
   intrinsic abs,max
   double precision ftol
   integer n
   real r(n)
   ftol(zx) = 0.5d0*max(abstol,reltol*abs(zx))
   r = -83.03
 entry dstzr()
   return
end

program entry_05
   real :: r(10)
   integer :: i
   call dzror(10, r)
   do i = 1, 10
      if (abs(r(i) - (-83.03)) > 1e-8) error stop
   end do
   print *, r
end program
