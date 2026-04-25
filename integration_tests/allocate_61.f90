program allocate_61
   use allocate_61_allocmod, only: alloc_source
   use allocate_61_mymod, only: w
   implicit none
   type(w) :: obj
   call alloc_source(obj)
   if (.not. allocated(obj%a)) error stop
   if (obj%a%x /= 10) error stop
   print *, "PASS"
end program
