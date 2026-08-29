module intent_out_finalize_05_mod
   implicit none
   integer :: g_order(8) = 0
   integer :: g_n = 0
   integer :: g_base_bid = -1
   integer :: g_child_cid = -1

   type :: base_t
      integer :: bid = 7
   contains
      final :: base_final
   end type base_t

   type, extends(base_t) :: mid_t   ! no finalizer of its own
      integer :: mid = 3
   end type mid_t

   type, extends(mid_t) :: child_t
      integer :: cid = 11
   contains
      final :: child_final
   end type child_t
contains
   subroutine base_final(this)
      type(base_t), intent(inout) :: this
      g_n = g_n + 1
      g_order(g_n) = 1
      g_base_bid = this%bid
   end subroutine base_final

   subroutine child_final(this)
      type(child_t), intent(inout) :: this
      g_n = g_n + 1
      g_order(g_n) = 3
      g_child_cid = this%cid
   end subroutine child_final

   subroutine reset(this)
      type(child_t), intent(out) :: this
   end subroutine reset
end module

program intent_out_finalize_05
   use intent_out_finalize_05_mod
   implicit none
   type(child_t) :: c

   c%bid = 100
   c%cid = 200

   call reset(c)

   if (g_n /= 2)            error stop 1
   if (g_order(1) /= 3)     error stop 2   ! child finalizer first
   if (g_order(2) /= 1)     error stop 3   ! ancestor finalizer next
   if (g_child_cid /= 200)  error stop 4   ! saw pre-reset child value
   if (g_base_bid /= 100)   error stop 5   ! saw pre-reset parent value

   ! intent(out) re-applies default initialization afterwards
   if (c%bid /= 7)          error stop 6
   if (c%cid /= 11)         error stop 7
   print *, "ok"
end program
