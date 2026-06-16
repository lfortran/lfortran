! Regression test for ICE: AssertFailed: arg_idx < func_subrout->n_args
! when calling this%caller(this%f, a1) — a procedure() pointer invoked with
! two arguments despite having no declared formal parameters.
module procedure_pointer_25_mod
   implicit none
   type :: method
      procedure(), nopass, pointer            :: f => null()
      procedure(), nopass, pointer, public    :: caller => null()
   contains
      procedure, pass(this), private :: invoke_a1
   end type

contains

   subroutine invoke_a1(this, a1)
      class(method), intent(inout)    :: this
      class(*), intent(in)            :: a1

      call this%caller(this%f, a1)
   end subroutine
end module

program procedure_pointer_25
   use procedure_pointer_25_mod
   implicit none

   type(method) :: m

   ! The ICE was triggered during codegen of the module above.
   ! Verify the type initializes correctly at runtime.
   if (associated(m%f)) error stop
   if (associated(m%caller)) error stop

   print *, "procedure_pointer_25: PASS"
end program
