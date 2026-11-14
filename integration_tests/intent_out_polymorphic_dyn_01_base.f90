module intent_out_polymorphic_dyn_01_base
   implicit none
   type :: base
      real, allocatable :: a(:)
   end type
   type :: plain
      integer :: n = 0
   end type
contains
   subroutine destroy(me)
      class(base), intent(out) :: me
   end subroutine
   subroutine destroy_optional(me)
      class(base), intent(out), optional :: me
   end subroutine
   subroutine destroy_plain(me)
      class(plain), intent(out) :: me
   end subroutine
   subroutine destroy_unlimited(me)
      class(*), intent(out) :: me
   end subroutine
   subroutine destroy_array(me)
      class(base), intent(out) :: me(:)
   end subroutine
end module
