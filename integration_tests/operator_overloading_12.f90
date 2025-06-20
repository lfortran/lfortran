module operator_overloading_12_module_1
  implicit none

  public :: operator(.and.)
  public :: operator(.or.)
  ! TODO later
  !public :: operator(.eor.)
  public :: operator(.eqv.)

  interface operator(.and.)
    function and(lhs, rhs) result(diagnosis)
      implicit none
      integer, intent(in) :: lhs, rhs
      integer diagnosis
    end function
  end interface

  interface operator(.or.)
    function or(lhs, rhs) result(diagnosis)
      implicit none
      integer, intent(in) :: lhs, rhs
      integer diagnosis
    end function
  end interface

  interface operator(.eqv.)
    function eqv(lhs, rhs) result(diagnosis)
      implicit none
      integer, intent(in) :: lhs, rhs
      integer diagnosis
    end function
  end interface

end module operator_overloading_12_module_1

function and(lhs, rhs) result(diagnosis)
  implicit none
  integer, intent(in) :: lhs, rhs
  integer :: diagnosis
  ! bitwise AND operation
  diagnosis = iand(lhs, rhs)
end function and

function or(lhs, rhs) result(diagnosis)
  implicit none
  integer, intent(in) :: lhs, rhs
  integer :: diagnosis
  diagnosis = ior(lhs, rhs)
end function

function eqv(lhs, rhs) result(diagnosis)
  implicit none
  integer, intent(in) :: lhs, rhs
  integer :: diagnosis
  diagnosis = 0
  if (lhs == rhs) diagnosis = 1
end function

program operator_overloading_12
  use operator_overloading_12_module_1
  implicit none
  integer :: a, b, result

  a = 15
  b = 10
  result = a .and. b
  print *, '15 .and. 10 = ', result
  if (result /= 10) error stop

  result = a .or. b
  print *, "15 .or. 10 = ", result
  if (result /= 15) error stop

  ! TODO: the below doesn't give correct output
  ! result = a .eqv. b
  ! print *, "15 .eqv. 10 = ", result
  ! if (result /= 0) error stop

  ! a = 10
  ! b = 10
  ! result = a .eqv. b
  ! print *, "10 .eqv. 10 = ", result
  ! if (result /= 1) error stop
end program operator_overloading_12
