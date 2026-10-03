module m_defined_op_match_02
   implicit none
   private
   public :: operator(.in.)

   interface operator(.in.)
      ! Elemental scalar overload declared BEFORE the rank-1 overload.
      module procedure in_scalar
      module procedure in_list
   end interface

contains

   elemental function in_scalar(a, b) result(res)
      integer, intent(in) :: a, b
      logical :: res
      res = a == b
   end function in_scalar

   function in_list(a, b) result(res)
      integer, intent(in) :: a
      integer, intent(in) :: b(:)
      logical :: res
      res = any(a == b)
   end function in_list

end module m_defined_op_match_02


program defined_op_match_02
   use m_defined_op_match_02, only: operator(.in.)
   implicit none

   integer :: arr(3) = [10, 20, 30]
   logical :: r

   ! scalar .in. rank-1 array must resolve to the non-elemental in_list
   ! (scalar logical result), not to the elemental in_scalar (which would
   ! broadcast to a logical array and be rejected by the if statement).
   if (.not. (20 .in. arr)) error stop "array whole"
   if (25 .in. arr) error stop "array whole (absent)"
   if (.not. (30 .in. arr(1:3))) error stop "array section"
   if (10 .in. arr(2:3)) error stop "array section (absent)"

   ! scalar .in. scalar must still resolve to the elemental in_scalar.
   r = (20 .in. 20)
   if (.not. r) error stop "scalar equal"
   if (20 .in. 21) error stop "scalar not equal"

   print *, "ok"
end program defined_op_match_02
