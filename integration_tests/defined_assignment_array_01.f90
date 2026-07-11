! F2023 10.2.1.4: non-elemental defined assignment(=) with scalar dummies does
! not apply to array LHS (ranks disagree). Array assignment is therefore
! intrinsic; componentwise assign leaves hit=.false.
!
! LFortran previously incorrectly called assign_t for each element.

module defined_assignment_array_01_mod
   implicit none
   type :: t
      integer :: v = 0
      logical :: hit = .false.
   contains
      procedure :: assign_t
      generic :: assignment(=) => assign_t
   end type t
contains
   subroutine assign_t(lhs, rhs)
      class(t), intent(inout) :: lhs
      class(t), intent(in)    :: rhs
      lhs%v = rhs%v
      lhs%hit = .true.   ! only set if defined assignment runs
   end subroutine assign_t
end module defined_assignment_array_01_mod

program defined_assignment_array_01
   use defined_assignment_array_01_mod
   implicit none
   type(t) :: a(2), b(2)
   integer :: i

   do i = 1, 2
      a(i)%v = i * 10
      a(i)%hit = .false.
      b(i)%hit = .false.
   end do

   b = a   ! rank-1 actuals, scalar dummies, non-elemental -> intrinsic

   print *, "b%v  =", b%v
   print *, "b%hit=", b%hit
   if (all(b%hit)) then
      print *, "RESULT: defined assignment was called (LFortran)"
      error stop 1
   else
      print *, "RESULT: defined assignment was NOT called (Flang/GFortran)"
   end if

   ! Values must still be assigned intrinsically
   if (b(1)%v /= 10 .or. b(2)%v /= 20) error stop 2

   ! Scalar defined assignment still applies
   b(1)%hit = .false.
   b(1) = a(2)
   if (b(1)%v /= 20) error stop 3
   if (.not. b(1)%hit) error stop 4
end program defined_assignment_array_01
