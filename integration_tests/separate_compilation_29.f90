subroutine other_subroutine(a)
   real(8), allocatable, intent(inout) :: a(:,:)
   allocate(a(3, 2))
   a(1,1) = 1.0d0
   a(2,1) = 2.0d0
   a(3,1) = 3.0d0
   a(1,2) = 4.0d0
   a(2,2) = 5.0d0
   a(3,2) = 6.0d0
end subroutine other_subroutine

subroutine some_subroutine()
   real(8), allocatable :: a(:,:)
   call other_subroutine(a)
   if (size(a, 1) /= 3) error stop
   if (size(a, 2) /= 2) error stop
   if (abs(a(1,1) - 1.0d0) > 1.0d-10) error stop
   if (abs(a(3,2) - 6.0d0) > 1.0d-10) error stop
end subroutine some_subroutine

program separate_compilation_29
   implicit none
   call some_subroutine()
   print *, "PASS"
end program separate_compilation_29
