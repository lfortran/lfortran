! Shape conformance is a rule of intrinsic assignment only. A defined
! assignment passes both sides as actual arguments, so the RHS may be an array
! constructor assigned to a scalar, or an array whose rank or size differs from
! the target. LFortran used to apply the intrinsic-assignment checks regardless
! of whether the statement resolved to a defined assignment(=).
! See https://github.com/lfortran/lfortran/issues/12665
!
! Every case below is rejected without the fix.

module defined_assignment_02_mod
   implicit none

   type :: bitset_t
      integer :: bits = 0
   end type bitset_t

   type :: tag_t
      integer :: n = 0
   end type tag_t

   interface assignment(=)
      module procedure assign_logical_to_bitset
      module procedure assign_int_to_bitset
      module procedure assign_int_to_tag_1d
      module procedure assign_real_to_tag_2d
   end interface

contains

   subroutine assign_logical_to_bitset(self, v)
      type(bitset_t), intent(out) :: self
      logical, intent(in) :: v(:)
      integer :: i
      self%bits = 0
      do i = 1, size(v)
         if (v(i)) self%bits = ibset(self%bits, i - 1)
      end do
   end subroutine assign_logical_to_bitset

   subroutine assign_int_to_bitset(self, v)
      type(bitset_t), intent(out) :: self
      integer, intent(in) :: v(:)
      self%bits = sum(v)
   end subroutine assign_int_to_bitset

   subroutine assign_int_to_tag_1d(self, v)
      type(tag_t), intent(out) :: self(:)
      integer, intent(in) :: v(:)
      self(:)%n = size(v)
   end subroutine assign_int_to_tag_1d

   subroutine assign_real_to_tag_2d(self, v)
      type(tag_t), intent(out) :: self(:,:)
      real, intent(in) :: v(:)
      self(:,:)%n = size(v)
   end subroutine assign_real_to_tag_2d

end module defined_assignment_02_mod

program defined_assignment_02
   use defined_assignment_02_mod
   implicit none

   type(bitset_t) :: x
   type(tag_t) :: a(5), b(2,3)
   integer :: iv(2) = [1, 2]
   real :: rv(4) = [1.0, 2.0, 3.0, 4.0]

   ! Array constructor as the RHS of a defined assignment to a scalar target
   x = [.true., .false., .true., .false.]
   if (x%bits /= 5) error stop 1

   ! The generic still selects on the constructor's type
   x = [1, 2, 3, 4]
   if (x%bits /= 10) error stop 2

   ! Array target, same rank as the RHS but a different size
   a = iv
   if (any(a%n /= 2)) error stop 3

   ! Array target of a different rank than the RHS
   b = rv
   if (any(b%n /= 4)) error stop 4

end program defined_assignment_02
