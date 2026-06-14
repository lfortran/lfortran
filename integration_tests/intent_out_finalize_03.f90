module intent_out_finalize_03_mod
   implicit none
   integer :: g_child_final_count = 0
   integer :: g_last_finalized_bid = -1
   integer :: g_last_finalized_cid = -1

   type :: base_t
      integer :: bid = 7
   contains
      final :: base_final
   end type base_t

   type, extends(base_t) :: child_t
      integer :: cid = 11
   contains
      final :: child_final
   end type child_t
contains
   subroutine base_final(this)
      type(base_t), intent(inout) :: this
      g_last_finalized_bid = this%bid
   end subroutine base_final

   subroutine child_final(this)
      type(child_t), intent(inout) :: this
      g_child_final_count = g_child_final_count + 1
      g_last_finalized_cid = this%cid
   end subroutine child_final

   subroutine reset(this)
      type(child_t), intent(out) :: this
   end subroutine reset
end module

program intent_out_finalize_03
   use intent_out_finalize_03_mod
   implicit none
   type(child_t) :: c

   c%bid = 100
   c%cid = 200

   call reset(c)

   if (g_child_final_count /= 1) error stop 1
   if (g_last_finalized_cid /= 200) error stop 2

   if (c%bid /= 7)  error stop 3
   if (c%cid /= 11) error stop 4
   print *, "ok"
end program
