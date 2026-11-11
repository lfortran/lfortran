module intent_out_polymorphic_dyn_01_ext
   use intent_out_polymorphic_dyn_01_base
   implicit none
   type, extends(base) :: ext
      real, allocatable :: b(:)
      character(:), allocatable :: s
   end type
   type, extends(ext) :: ext2
      integer, allocatable :: c
      type(ext) :: inner
   end type
   type, extends(plain) :: plain_ext
      real, allocatable :: d(:)
   end type
contains
   subroutine init(me)
      class(ext), intent(inout) :: me
      call destroy(me)
      allocate (me%b(2))
      me%b = 1.0
      me%s = "abc"
   end subroutine
end module
