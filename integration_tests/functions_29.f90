program function_29
   implicit none

   integer, allocatable :: x(:)

   allocate(x(2))
   x = [1, 2]
   call my_func(x)

contains

   subroutine my_func(arr)
      integer, intent(in) :: arr(2)
      print *, arr
      if (any(arr /= [1, 2])) error stop
   end subroutine my_func

end program function_29

