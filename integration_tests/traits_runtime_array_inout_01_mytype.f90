module mytype_mod

   abstract interface  :: IEstimates
   end interface
   
   abstract interface :: ICoeffs
      subroutine update(self,estimates,beta)
         type(itself), pass, intent(inout) :: self
         class(IEstimates),  intent(inout) :: estimates
         real(8),            intent(inout) :: beta(:)
      end subroutine update
   end interface
     
   real(8) :: beta(12)

   type :: MyType
      class(ICoeffs),    allocatable :: coeffs
      class(IEstimates), allocatable :: estimates
   contains
      procedure :: coefficients
   end type MyType

contains
   
   subroutine coefficients(self)
      class(MyType), intent(inout) :: self
      call self%coeffs%update(self%estimates,beta)
   end subroutine coefficients

end module mytype_mod
