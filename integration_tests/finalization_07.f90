module finalization_07_mod
   implicit none
   integer :: final_count = 0
   integer :: last_tag = -1

   type :: base_t
   end type base_t

   type, extends(base_t) :: child_t
      integer, allocatable :: payload(:)
   contains
      final :: finalise_child
   end type child_t

contains

   subroutine finalise_child(this)
      type(child_t), intent(inout) :: this
      final_count = final_count + 1
      if (allocated(this%payload)) then
         last_tag = this%payload(1)
         deallocate(this%payload)
      end if
   end subroutine finalise_child

   function make_child(n) result(c)
      integer, intent(in) :: n
      type(child_t) :: c
      allocate(c%payload(1))
      c%payload(1) = n
   end function make_child

end module finalization_07_mod

program finalization_07
   use finalization_07_mod
   implicit none
   class(base_t), allocatable :: a
   integer :: before_dealloc

   a = make_child(42)

   before_dealloc = final_count
   deallocate(a)
   if (final_count <= before_dealloc) then
      error stop "FINAL was not invoked by deallocate(polymorphic_allocatable)"
   end if
   if (last_tag /= 42) error stop "FINAL saw wrong payload value"
end program finalization_07
