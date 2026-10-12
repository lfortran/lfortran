! The paper's client of the intrinsic module generics (traits.tex,
! "Predefined traits"): ISum is constrained by the predefined INumeric, the
! closed type set integer(*) | real(*) | complex(*). Adder implements it with
! a generic procedure checked once against every member.
module traits_generics_module_01_sums
   use, intrinsic :: generics, only: INumeric
   implicit none
   private
   public :: ISum, Adder, total

   abstract interface :: ISum
      function sum{INumeric :: T}(x) result(s)
         type(T), intent(in) :: x(:)
         type(T)             :: s
      end function sum
   end interface ISum

   type, sealed, implements(ISum) :: Adder
   contains
      procedure, nopass :: sum => total
   end type Adder

contains

   function total{INumeric :: T}(x) result(s)
      type(T), intent(in) :: x(:)
      type(T)             :: s
      integer             :: i
      s = T(0)
      do i = 1, size(x)
         s = s + x(i)
      end do
   end function total
end module traits_generics_module_01_sums
