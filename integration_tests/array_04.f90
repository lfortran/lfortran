program array_04
   implicit none

   real :: arr(5)
   integer :: i

   ! Initialize array
   do i = 1, 5
      arr(i) = i * 1.0
   end do

   call sub(arr, 5)

end program array_04


subroutine sub(a, n)
   implicit none

   real a(*)
   integer n

   ! Basic validation
   if (n <= 0) then
      error stop "Error: n must be positive"
   end if

   call nested()

contains

   subroutine nested()
      integer i

      ! Additional safety check (optional)
      if (n <= 0) then
         error stop "Error inside nested: invalid n"
      end if

      if (a(1) /= 1.0 .or. a(2) /= 2.0 .or. a(3) /= 3.0 .or. a(4) /= 4.0 .or. a(5) /= 5.0) then
         error stop "Error inside nested: array elements are not as expected"
      end if

   end subroutine nested

end subroutine sub
