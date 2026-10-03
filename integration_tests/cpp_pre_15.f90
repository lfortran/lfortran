#define mymacro(x) (x + 1)

module cpp_pre_15_mod
  implicit none
contains
  subroutine sub(x, mymacro)
    integer, intent(in) :: x
    integer, intent(in) :: mymacro
    if (x /= 1) error stop
    if (mymacro /= 43) error stop
  end subroutine
end module

program cpp_pre_15
  use cpp_pre_15_mod
  implicit none
  integer :: mymacro, r

  ! FLM name used as a variable (not followed by '(' so no expansion)
  mymacro = 10
  if (mymacro /= 10) error stop

  ! FLM invocation with ( expands normally
  r = mymacro(42)
  if (r /= 43) error stop

  ! FLM with spaces before ( still expands
  r = mymacro  (99)
  if (r /= 100) error stop

  ! FLM name at end of source line (next char is newline, not '(')
  r = &
      mymacro
  if (r /= 10) error stop

  ! FLM name as subroutine dummy argument name
  call sub(1, mymacro(42))

  print *, "PASS"
end program
