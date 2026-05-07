! Regression: a `class is(T)` block inside `select rank(rank=0)` creates a
! typed associate variable whose LLVM storage is a class wrapper pointer
! `%T_class*`. At scope exit lfortran used to call the user FINAL procedure
! `finalise(class_wrapper*)`, but the user finalizer expects `type(T)*` and
! the LLVM verifier rejects the call signature.
!
! This test exercises the rank(0) `class is` codegen path while keeping the
! actual call dispatched to the rank(2) branch so the program also runs.
module mod_finalization_05
   implicit none
   type :: array_type
      integer :: id = -1
   contains
      final :: finalise_array
   end type array_type
contains
   recursive subroutine finalise_array(this)
      type(array_type), intent(inout) :: this
      this%id = -2
   end subroutine finalise_array

   subroutine use_one(this)
      type(array_type), intent(in) :: this
      if (this%id < 0) error stop "use_one received invalid id"
   end subroutine use_one

   subroutine save_input(input, hits)
      class(*), dimension(..), intent(in) :: input
      integer, intent(out) :: hits
      hits = 0
      select rank(input)
      rank(0)
         select type(input)
         class is (array_type)
            call use_one(input)
            hits = 1
         end select
      rank(2)
         hits = 2
      end select
   end subroutine save_input
end module mod_finalization_05

program finalization_05
   use mod_finalization_05
   implicit none
   type(array_type) :: a(2,2)
   integer :: hits
   call save_input(a, hits)
   if (hits /= 2) error stop "wrong rank dispatch"
   print *, "ok"
end program finalization_05
