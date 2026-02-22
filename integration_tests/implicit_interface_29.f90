subroutine some_subroutine()
   real(8), allocatable :: a(:,:)
   allocate(a(3, 3))
   a = 1.0d0
   a(2,2) = 5.0d0
   call other_subroutine(a, 3, 3)
   if (a(1,1) /= 1.0d0) error stop
   if (a(2,2) /= 5.0d0) error stop
end subroutine some_subroutine

program implicit_interface_29
   implicit none
   call some_subroutine()
   print *, "ok"
end program implicit_interface_29
