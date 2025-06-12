program class_33
    implicit none

    type :: val_type
       integer :: origin = 3
    end type

   class(val_type), allocatable :: val1
   allocate(val1)
   call sub(val1)
   print *, "Outside val1%origin: ", val1%origin

   if (val1%origin /= 5) error stop

contains

   subroutine sub(val)
      type(val_type), intent(inout) :: val
      print *, "Inside subroutine, val%origin: ", val%origin
      if (val%origin /= 3) error stop
      val%origin = 5
   end subroutine sub

end program class_33
