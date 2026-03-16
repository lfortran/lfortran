! Test: generic procedure with keyword arg resolves to correct overload
! when an earlier overload has a required argument that is not supplied.
! Regression test for: ICE AssertFailed: f != nullptr in argument_types_match
! when a required arg is nullptr (not provided) for a non-matching overload.
!
! Part 1 (foo): required Variable_t arg is nullptr → must return false.
! Part 2 (bar): required Function_t (procedure dummy via interface block)
!   arg is nullptr → must return false (not crash in expr_type).

module generic_name_04_mod
  implicit none

  interface foo
    ! First overload: center is REQUIRED, mask is optional.
    ! A call that omits center should skip this overload.
    module procedure foo_center
    ! Second overload: center is optional, mask(:) is REQUIRED.
    ! A call foo(y, mask=m) should resolve to this.
    module procedure foo_mask
  end interface

  interface bar
    ! First overload: op is a REQUIRED procedure dummy (Function_t in ASR).
    ! A call that omits op should skip this overload.
    module procedure bar_with_op
    ! Second overload: op is an optional real (Variable_t in ASR).
    ! A call bar(x, n=3) should resolve to this.
    module procedure bar_simple
  end interface

contains

  real function foo_center(x, center, mask) result(res)
    real, intent(in) :: x(:)
    real, intent(in) :: center
    logical, intent(in), optional :: mask
    res = sum(x) + center
  end function

  real function foo_mask(x, center, mask) result(res)
    real, intent(in) :: x(:)
    real, intent(in), optional :: center
    logical, intent(in) :: mask(:)
    res = sum(x, mask=mask)
  end function

  real function bar_with_op(x, op, n)
    real, intent(in) :: x
    interface
      real function op(y)
        real, intent(in) :: y
      end function
    end interface
    integer, intent(in) :: n
    bar_with_op = op(x) * n
  end function

  real function bar_simple(x, op, n)
    real, intent(in) :: x
    real, intent(in), optional :: op
    integer, intent(in) :: n
    if (present(op)) then
      bar_simple = x * op * n
    else
      bar_simple = x * n
    end if
  end function

end module

program generic_name_04
  use generic_name_04_mod
  implicit none
  real    :: y(6) = [1., 2., 3., 4., 5., 6.]
  logical :: m(6)
  real    :: r

  ! Part 1: Variable_t nullptr path
  m = y > 3.
  ! Should resolve to foo_mask: sum of elements > 3 = 4+5+6 = 15
  r = foo(y, mask=m)
  print *, r
  if (abs(r - 15.0) > 1e-5) error stop

  ! Part 2: Function_t nullptr path
  ! Call bar(2.0, n=3) — 'op' not provided.
  ! Overload 1 (bar_with_op): op is Function_t, required → must skip.
  ! Overload 2 (bar_simple): op is optional real → matches. Result = 2.0 * 3 = 6.0.
  r = bar(2.0, n=3)
  print *, r
  if (abs(r - 6.0) > 1e-5) error stop
end program
