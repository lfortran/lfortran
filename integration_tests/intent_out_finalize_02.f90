module intent_out_finalize_02_mod
   implicit none
   integer :: g_freed_id = -1
   type :: t
      integer :: id          = 0
      logical :: owns_resource = .false.
      type(t), pointer :: resource => null()
   contains
      procedure :: assign_t
      generic   :: assignment(=) => assign_t
      final     :: finalize_t
   end type t
contains
   recursive subroutine finalize_t(this)
      type(t), intent(inout) :: this
      if (this%owns_resource .and. associated(this%resource)) then
         g_freed_id = this%resource%id
         deallocate(this%resource)
      end if
      nullify(this%resource)
      this%owns_resource = .false.
   end subroutine finalize_t

   recursive subroutine assign_t(this, src)
      class(t), intent(out) :: this
      type(t),  intent(in)  :: src
      this%id              = src%id
      this%owns_resource   = src%owns_resource
      if (associated(src%resource)) this%resource => src%resource
   end subroutine assign_t
end module

program intent_out_finalize_02
   use intent_out_finalize_02_mod
   implicit none
   type(t) :: a, b

   a%id = 100
   allocate(a%resource)
   a%resource%id    = 555
   a%owns_resource  = .true.

   b%id            = 200
   b%owns_resource = .false.

   a = b

   if (g_freed_id /= 555) error stop 1
   if (a%id      /= 200) error stop 2
   print *, "ok"
end program
