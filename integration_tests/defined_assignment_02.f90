! Shape conformance is a rule of intrinsic assignment only. A defined
! assignment passes both sides as actual arguments, so the RHS may be an array
! constructor assigned to a scalar, or an array whose rank or size differs from
! the target. LFortran used to apply the intrinsic-assignment checks regardless
! of whether the statement resolved to a defined assignment(=).
! See https://github.com/lfortran/lfortran/issues/12665
!
! Every case below is rejected without the fix.
!
! The free interface and the type-bound generic live in separate modules, and
! each test procedure uses only one of them: a module-level `interface
! assignment(=)` in scope shadows type-bound assignment resolution (same
! restriction noted in defined_assignment_01.f90).

module defined_assignment_02_free_mod
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

end module defined_assignment_02_free_mod

module defined_assignment_02_tb_mod
   implicit none

   type :: counter_t
      integer :: n = 0
   contains
      procedure :: assign_counter
      generic :: assignment(=) => assign_counter
   end type counter_t

contains

   subroutine assign_counter(lhs, rhs)
      class(counter_t), intent(out) :: lhs
      integer, intent(in) :: rhs(:)
      lhs%n = size(rhs)
   end subroutine assign_counter

end module defined_assignment_02_tb_mod

program defined_assignment_02
   implicit none

   call test_scalar_target()
   call test_array_target()
   call test_type_bound()

   print *, "ok"

contains

   ! Array constructor assigned to a scalar target
   subroutine test_scalar_target()
      use defined_assignment_02_free_mod
      type(bitset_t) :: x

      x = [.true., .false., .true., .false.]
      if (x%bits /= 5) error stop 1

      ! The generic still selects on the constructor's type
      x = [1, 2, 3, 4]
      if (x%bits /= 10) error stop 2
   end subroutine test_scalar_target

   ! Array target whose shape does not conform to the RHS
   subroutine test_array_target()
      use defined_assignment_02_free_mod
      type(tag_t) :: a(5), b(2,3)
      integer :: iv(2) = [1, 2]
      real :: rv(4) = [1.0, 2.0, 3.0, 4.0]

      ! Same rank, different size
      a = iv
      if (any(a%n /= 2)) error stop 3

      ! Different rank
      b = rv
      if (any(b%n /= 4)) error stop 4
   end subroutine test_array_target

   ! Same, through a type-bound generic assignment(=)
   subroutine test_type_bound()
      use defined_assignment_02_tb_mod
      type(counter_t) :: c

      c = [10, 20, 30]
      if (c%n /= 3) error stop 5
   end subroutine test_type_bound

end program defined_assignment_02
