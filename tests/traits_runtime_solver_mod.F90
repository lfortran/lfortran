module solver_mod

   abstract interface :: IRhsFunc
      subroutine sfunc(x,y,yp)
         real(8), intent(in)  :: x
         real(8), intent(in)  :: y(:)
         real(8), intent(out) :: yp(:)
      end subroutine sfunc
   end interface
 
   abstract interface :: ISolver
      subroutine integrate(self,rhs,neqn,y,t,tout,relerr,abserr,iflag)
         type(itself), pass, intent(inout) :: self
         class(IRhsFunc), intent(in)       :: rhs
         integer                           :: neqn, iflag
         real(8)                           :: t, tout, relerr, abserr
         real(8)                           :: y(neqn)
      end subroutine integrate
   end interface

end module solver_mod
