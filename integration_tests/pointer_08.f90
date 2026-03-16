program pointer_08
! Test logical array pointer initialized to null()
! Verifies that declaring a logical pointer array with => null()
! compiles and runs correctly.
   implicit none
   logical, pointer :: p(:) => null()
   logical, pointer :: q(:,:) => null()
   logical(1), pointer :: r(:) => null()
   logical(4), pointer :: s(:) => null()

   if (associated(p)) error stop
   if (associated(q)) error stop
   if (associated(r)) error stop
   if (associated(s)) error stop

   print *, "All tests passed."
end program pointer_08
