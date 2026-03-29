module derived_types_133_m
   implicit none
   type :: t
      integer :: v
   contains
      final :: destroy
   end type t
contains
   subroutine destroy(self)
      type(t), intent(inout) :: self
      self%v = 0
   end subroutine
end module

program derived_types_133
   use derived_types_133_m
   implicit none
   type(t) :: a(3)
   a(1)%v = 10
   a(2)%v = 20
   a(3)%v = 30
   print *, a(1)%v
   print *, a(2)%v
   print *, a(3)%v
   if (a(1)%v /= 10) error stop
   if (a(2)%v /= 20) error stop
   if (a(3)%v /= 30) error stop
end program
