program pointer_07
! Test pointer bounds remapping to array sections
! Verifies that ap(0:n1,0:n2) => a(:,k) correctly points
! to the right column when reassociated
   implicit none
   integer :: i, j, n1, n2
   real(8), allocatable, target :: a(:,:)
   real(8), pointer :: ap(:,:) => null()

   n1 = 3
   n2 = 3

   allocate( a((n1+1)*(n2+1),2) )
   a = 0.0d0

   ! First association: ap => column 1
   ap(0:n1,0:n2) => a(:,1)

   do j = 0, n2
      do i = 0, n1
         ap(i,j) = 100.0d0 * j + i
      end do
   end do

   ! Second association: ap => column 2
   ap(0:n1,0:n2) => a(:,2)

   do j = 0, n2
      do i = 0, n1
         ap(i,j) = -(100.0d0 * j + i)
      end do
   end do

   ! Verify column 1 was written correctly
   if (abs(a(1,1) - 0.0d0) > 1.0d-10) error stop
   if (abs(a(2,1) - 1.0d0) > 1.0d-10) error stop
   if (abs(a(3,1) - 2.0d0) > 1.0d-10) error stop
   if (abs(a(4,1) - 3.0d0) > 1.0d-10) error stop
   if (abs(a(5,1) - 100.0d0) > 1.0d-10) error stop
   if (abs(a(6,1) - 101.0d0) > 1.0d-10) error stop
   if (abs(a(16,1) - 303.0d0) > 1.0d-10) error stop

   ! Verify column 2 was written correctly (negative values)
   if (abs(a(1,2) - 0.0d0) > 1.0d-10) error stop
   if (abs(a(2,2) - (-1.0d0)) > 1.0d-10) error stop
   if (abs(a(3,2) - (-2.0d0)) > 1.0d-10) error stop
   if (abs(a(4,2) - (-3.0d0)) > 1.0d-10) error stop
   if (abs(a(5,2) - (-100.0d0)) > 1.0d-10) error stop
   if (abs(a(16,2) - (-303.0d0)) > 1.0d-10) error stop

   print *, "All tests passed."

end program pointer_07
