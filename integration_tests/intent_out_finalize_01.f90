module intent_out_finalize_01_mod
   implicit none
   integer :: g_finalize_count = 0
   integer :: g_last_finalized_id = -1
   integer :: g_last_finalized_count = -1
   type :: t
      integer :: id   = 0
      integer :: tag  = 7         ! default-initialised scalar
   contains
      final :: finalize_t
   end type t
contains
   subroutine finalize_t(this)
      type(t), intent(inout) :: this
      g_finalize_count = g_finalize_count + 1
      g_last_finalized_id    = this%id
      g_last_finalized_count = this%tag
   end subroutine finalize_t

   subroutine reset(this, src)
      type(t), intent(out) :: this
      type(t), intent(in)  :: src
      this%id  = src%id
      this%tag = src%tag
   end subroutine reset
end module

program intent_out_finalize_01
   use intent_out_finalize_01_mod
   implicit none
   type(t) :: a, b

   a%id  = 100
   a%tag = 42
   b%id  = 200
   b%tag = 99

   call reset(a, b)

   if (g_finalize_count /= 1)         error stop 1
   if (g_last_finalized_id    /= 100) error stop 2
   if (g_last_finalized_count /= 42)  error stop 3

   if (a%id  /= 200) error stop 4
   if (a%tag /= 99)  error stop 5
   print *, "ok"
end program
