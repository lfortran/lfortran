! F2023 10.2.1.3: intrinsic array assignment of an extended type assigns the
! parent component with type-bound defined assignment when present, and
! extension components intrinsically.
!
! Array b = a (child_t): call parent assignment(=) per element + copy tag.
! Scalar b(1) = a(2): defined assignment only (tag untouched).

module defined_assignment_parent_array_01_mod
   implicit none
   type :: parent_t
      integer :: v = 0
      logical :: hit = .false.
   contains
      procedure :: assign_parent
      generic :: assignment(=) => assign_parent
   end type parent_t

   type, extends(parent_t) :: child_t
      integer :: tag = 0
   end type child_t
contains
   subroutine assign_parent(lhs, rhs)
      class(parent_t), intent(inout) :: lhs
      class(parent_t), intent(in)    :: rhs
      lhs%v = rhs%v
      lhs%hit = .true.
   end subroutine assign_parent
end module defined_assignment_parent_array_01_mod

program defined_assignment_parent_array_01
   use defined_assignment_parent_array_01_mod
   implicit none
   type(child_t) :: a(2), b(2)
   integer :: i

   do i = 1, 2
      a(i)%v = i * 10
      a(i)%hit = .false.
      a(i)%tag = i
      b(i)%hit = .false.
      b(i)%tag = -1
   end do

   ! Intrinsic array assignment: parent component via defined assignment
   b = a
   do i = 1, 2
      if (b(i)%v /= i * 10) error stop 1
      if (.not. b(i)%hit) error stop 2
      if (b(i)%tag /= i) error stop 3
   end do

   ! Scalar defined assignment: only parent fields updated
   b(1)%hit = .false.
   b(1)%tag = -99
   b(1) = a(2)
   if (b(1)%v /= 20) error stop 4
   if (.not. b(1)%hit) error stop 5
   if (b(1)%tag /= -99) error stop 6
end program defined_assignment_parent_array_01
