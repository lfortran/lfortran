! Regression test for ICE: AssertFailed: arg_idx < func_subrout->n_args
! when calling this%caller(this%f, a1) — a procedure() pointer invoked with
! two arguments despite having no declared formal parameters.
module procedure_pointer_25_mod
   implicit none
   integer :: global_result = 0

   type :: method
      procedure(), nopass, pointer            :: f => null()
      procedure(), nopass, pointer, public    :: caller => null()
   contains
      procedure, pass(this) :: run
   end type

contains

   subroutine run(this, val)
      class(method), intent(inout) :: this
      integer, intent(in) :: val
      ! This is the core pattern that triggered the ICE:
      ! this%caller is procedure() with n_args=0, but we pass 2 arguments.
      call this%caller(this%f, val)
   end subroutine
end module

program procedure_pointer_25
   use procedure_pointer_25_mod
   implicit none

   type(method) :: m

   ! Test 1: Wire up caller and f, then invoke through the type
   m%f => double_it
   m%caller => my_caller
   call m%run(21)
   if (global_result /= 42) error stop

   ! Test 2: Different input value
   call m%run(50)
   if (global_result /= 100) error stop

   ! Test 3: Zero input
   call m%run(0)
   if (global_result /= 0) error stop

   print *, "procedure_pointer_25: PASS"

contains

   subroutine my_caller(f_ptr, val)
      procedure() :: f_ptr
      integer, intent(in) :: val
      call f_ptr(val)
   end subroutine

   subroutine double_it(n)
      integer, intent(in) :: n
      global_result = n * 2
   end subroutine

end program
