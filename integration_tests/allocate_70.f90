! Regression: `allocate(T :: arr(n))` for an unlimited polymorphic
! allocatable lowered to `allocate_array_of_classes` with a class wrapper
! whose data field is `i8*`. The codegen used to bitcast the freshly
! malloc'd `i8*` buffer to the concrete struct type before storing it
! into the wrapper's data slot, which produced
!   `store %T*, i8** %slot`
! and the LLVM verifier rejected the module. The store should keep the
! buffer typed as `i8*` for unlimited polymorphic wrappers.
module mod_allocate_70
   implicit none
   type :: graph_type
      integer :: id = -1
   end type graph_type
contains
   subroutine handle(n, ok)
      integer, intent(in) :: n
      integer, intent(out) :: ok
      class(*), allocatable :: arr(:)
      ok = 0
      allocate(graph_type :: arr(n))
      if (.not. allocated(arr)) return
      ok = 1
   end subroutine handle
end module mod_allocate_70

program allocate_70
   use mod_allocate_70
   implicit none
   integer :: ok
   call handle(3, ok)
   if (ok /= 1) error stop "allocate failed"
   print *, "ok"
end program allocate_70
