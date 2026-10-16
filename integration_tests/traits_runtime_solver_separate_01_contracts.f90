! The interfaces of traits_runtime_solver_separate_01, as in the user's module:
! IRhsFunc's sfunc marks no receiver and has an intent(out) derivative, and
! ISolver's integrate borrows a read-only IRhsFunc view and takes the
! explicit-shape y(neqn) and scalars without INTENT.
module traits_runtime_solver_separate_01_contracts_m

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

end module traits_runtime_solver_separate_01_contracts_m
