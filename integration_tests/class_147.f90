program class_147
   implicit none

   type :: m_type
      character(len=10) :: c
   end type

   class(m_type), allocatable :: value(:)
   integer :: i

   allocate(value(3))
   do i = 1, 3
      value(i)%c = "test"
   end do
   do i = 1, 3
      if (value(i)%c /= "test") error stop
   end do
   deallocate(value)

   allocate(value(5))
   do i = 1, 5
      value(i)%c = "abc"
   end do
   do i = 1, 5
      if (value(i)%c /= "abc") error stop
   end do
   deallocate(value)

   print *, "PASSED"
end program
