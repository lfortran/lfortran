program intent_out_nested_allocatable_01
   implicit none

   type tt2
      integer, allocatable :: xx
   end type tt2
   
   type tt
      type(tt2) :: internal
   end type
   
   type(tt) :: instance

   allocate(instance%internal%xx)
   call ss(instance)

contains 
   subroutine ss(kk)
      type(tt), intent(out) :: kk
      if (allocated(kk%internal%xx)) then
          error stop "Test failed: intent(out) did not deallocate nested allocatable."
      end if
      print *, "Test passed: intent(out) successfully deallocated nested allocatable."
   end subroutine
end program
