module mytype_mod

   implicit none
   private
   
   public :: MyType

   abstract interface :: IEstimates
      subroutine some_method(self)
         type(itself), pass, intent(inout) :: self
      end subroutine some_method
   end interface
   
   abstract interface :: ICoeffs
      subroutine update(self,estimates)
         type(itself), pass, intent(inout) :: self
         class(IEstimates),  intent(inout) :: estimates
      end subroutine update
   end interface
   
   type :: MyType
      private
      class(ICoeffs),    allocatable :: coeffs
      class(IEstimates), allocatable :: estimates
   contains
      procedure, private :: coefficients
   end type MyType
   
contains

   subroutine coefficients(self)
      class(MyType), intent(inout) :: self
      call self%coeffs%update(self%estimates)
   end subroutine coefficients

end module mytype_mod
