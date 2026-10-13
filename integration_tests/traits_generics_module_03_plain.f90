! A USE of generics without a module nature: no module of the program is
! named generics, so it is the intrinsic module, as for iso_fortran_env.
module traits_generics_module_03_plain
   use generics, only: INumeric
   implicit none
   private
   public :: plain_total

contains

   function plain_total{INumeric :: T}(x) result(s)
      type(T), intent(in) :: x(:)
      type(T)             :: s
      integer             :: i
      s = T(0)
      do i = 1, size(x)
         s = s + x(i)
      end do
   end function plain_total
end module traits_generics_module_03_plain
