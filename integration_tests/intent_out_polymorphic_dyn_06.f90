module intent_out_polymorphic_dyn_06_m
   implicit none
   integer :: n_final = 0
   type :: b
      integer :: h = 4
   contains
      final :: fin
   end type
   type, extends(b) :: e
      integer :: extra = 9
      integer, allocatable :: y(:)
   end type
contains
   subroutine fin(x)
      type(b), intent(inout) :: x
      n_final = n_final + 1
   end subroutine
   subroutine s(v)
      class(b), intent(out) :: v
   end subroutine
   subroutine sa(v)
      class(b), intent(out) :: v(:)
   end subroutine
end module

program intent_out_polymorphic_dyn_06
   use intent_out_polymorphic_dyn_06_m
   implicit none
   type(e) :: c, a(2)

   ! The declared type has a final subroutine: the extension's components
   ! are still deallocated and default-initialized.
   c%h = 1
   c%extra = 1
   allocate (c%y(2))
   call s(c)
   if (c%h /= 4 .or. c%extra /= 9 .or. allocated(c%y)) error stop 1
   if (n_final /= 1) error stop 2

   a(1)%extra = 1
   a(2)%h = 2
   allocate (a(2)%y(1))
   call sa(a)
   if (any(a%h /= 4) .or. any(a%extra /= 9)) error stop 3
   if (allocated(a(2)%y)) error stop 4

   print *, "ok"
end program
