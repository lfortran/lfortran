! A defined assignment passes its right-hand side as an actual argument, so an
! array constructor is a valid RHS even when the left-hand side is a scalar
! (F2018 10.2.1.4); only intrinsic assignment requires conforming shapes.

module defined_assignment_02_mod
   implicit none

   type :: bitset_t
      integer :: bits = 0
   end type bitset_t

   interface assignment(=)
      module procedure assign_logical_to_bitset
      module procedure assign_integer_to_bitset
   end interface

contains

   subroutine assign_logical_to_bitset(self, mask)
      type(bitset_t), intent(out) :: self
      logical, intent(in) :: mask(:)
      integer :: i
      self%bits = 0
      do i = 1, size(mask)
         if (mask(i)) self%bits = ibset(self%bits, i - 1)
      end do
   end subroutine assign_logical_to_bitset

   subroutine assign_integer_to_bitset(self, values)
      type(bitset_t), intent(out) :: self
      integer, intent(in) :: values(:)
      self%bits = sum(values)
   end subroutine assign_integer_to_bitset

end module defined_assignment_02_mod

program defined_assignment_02
   use defined_assignment_02_mod
   implicit none

   type(bitset_t) :: x
   logical :: mask(4) = [.true., .false., .true., .false.]
   integer :: i

   ! Array constructor as the RHS of a defined assignment to a scalar
   x = [.true., .false., .true., .false.]
   if (x%bits /= 5) error stop 1

   ! A named array RHS resolves to the same specific procedure
   x = mask
   if (x%bits /= 5) error stop 2

   ! An implied-do array constructor is accepted too
   x = [(mod(i, 2) == 1, i = 1, 4)]
   if (x%bits /= 5) error stop 3

   ! The generic still selects on the constructor's type
   x = [1, 2, 3]
   if (x%bits /= 6) error stop 4

   ! An empty constructor is a zero-sized actual argument
   x = [integer ::]
   if (x%bits /= 0) error stop 5

end program defined_assignment_02
