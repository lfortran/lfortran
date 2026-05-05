program optional_12
   implicit none
   integer, allocatable, dimension(:) :: arr
   logical :: ok

   allocate(arr(3))
   arr = [10, 20, 30]

   call sub(arr, ok)
   if (.not. ok) error stop "expected present to be true"

   call check_unallocated()

   print *, "ok"

contains

   subroutine sub(x, was_present)
      integer, dimension(..), optional, intent(in) :: x
      logical, intent(out) :: was_present
      was_present = present(x)
      if (present(x)) then
         select rank(x)
         rank(0)
            error stop "unexpected rank 0"
         rank(1)
            if (size(x) /= 3) error stop "wrong size"
            if (x(1) /= 10) error stop "wrong value at 1"
            if (x(2) /= 20) error stop "wrong value at 2"
            if (x(3) /= 30) error stop "wrong value at 3"
         rank default
            error stop "unexpected rank"
         end select
      end if
   end subroutine sub

   subroutine check_unallocated()
      integer, allocatable, dimension(:) :: empty
      logical :: present_flag
      call sub(empty, present_flag)
      if (present_flag) error stop "expected present to be false for unallocated"
   end subroutine check_unallocated

end program optional_12
