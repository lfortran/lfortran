module finalization_08_mod
   implicit none
   integer :: final_count = 0
   integer :: last_seen_id = -1

   type :: t
      integer :: id = 0
   contains
      final :: fin_t
   end type t

contains

   subroutine fin_t(this)
      type(t), intent(inout) :: this
      final_count = final_count + 1
      last_seen_id = this%id
   end subroutine fin_t

   function make_t() result(r)
      type(t) :: r
      if (final_count /= 0) then
         error stop "fin_t was invoked on the function result before body executed"
      end if
      if (r%id /= 0) then
         error stop "result variable not default-initialized"
      end if
      r%id = 99
   end function make_t

end module finalization_08_mod

program finalization_08
   use finalization_08_mod
   implicit none
   type(t) :: x

   if (final_count /= 0) error stop "unexpected fin_t call before assignment"
   x = make_t()
   if (x%id /= 99) error stop "result was not propagated to caller"
end program finalization_08
