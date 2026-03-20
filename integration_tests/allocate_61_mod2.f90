module allocate_61_allocmod
   use allocate_61_mymod, only: w, make_t
   implicit none
contains
   subroutine alloc_source(x)
      type(w), intent(inout) :: x
      allocate(x%a, source=make_t())
   end subroutine
end module
