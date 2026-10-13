! A second, separately compiled client of the intrinsic module generics. It
! imports INumeric under another name; forwarding to total, constrained by
! the INumeric of traits_generics_module_01_sums, is accepted because both
! name the one trait that the intrinsic module declares.
module traits_generics_module_01_ops
   use, intrinsic :: generics, only: Numeric => INumeric
   use traits_generics_module_01_sums, only: total
   implicit none
   private
   public :: twice

contains

   function twice{Numeric :: T}(x) result(r)
      type(T), intent(in) :: x(:)
      type(T)             :: r
      r = total(x) + total{T}(x)
   end function twice
end module traits_generics_module_01_ops
