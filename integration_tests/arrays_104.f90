! Test passing non-contiguous array sections to type-bound procedures.
! The stride of the second dimension must reflect the original array's
! leading dimension, not the extent of the section.
module arrays_104_mod
   implicit none

   type :: MyType
   contains
      procedure :: method
   end type MyType

contains

   subroutine method(self, a, b)
      class(MyType), intent(in) :: self
      real(8), intent(in) :: a(:,:)
      real(8), intent(in) :: b(:)
      integer :: i, j

      do i = 1, size(a, 1)
         do j = 1, size(a, 2)
            if (abs(a(i, j) - real(j, 8)) > 1.0d-12) error stop
         end do
      end do

      do i = 1, size(b)
         if (abs(b(i) - 1.0d0) > 1.0d-12) error stop
      end do
   end subroutine method

end module arrays_104_mod

program arrays_104
   use arrays_104_mod
   implicit none

   real(8) :: a(4, 4), b(4)
   integer :: i, j, n, n2
   class(MyType), allocatable :: obj

   allocate(obj)

   n = 3
   n2 = 4

   do j = 1, 4
      do i = 1, 4
         a(i, j) = real(j, 8)
      end do
   end do

   b = 1.0d0

   ! Verify directly
   do i = 1, n
      do j = 1, n2
         if (abs(a(i, j) - real(j, 8)) > 1.0d-12) error stop
      end do
   end do

   ! Pass non-contiguous section a(1:3, 1:4) from a 4x4 array
   call obj%method(a(1:n, 1:n2), b(1:n))

   print *, "PASS"
end program arrays_104
