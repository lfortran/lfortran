module class_88_m
  type t
    integer :: n = 0
   contains
    procedure :: tbp => f
  end type
 contains
  function f(this)
    class(t), pointer, intent(in) :: this
    integer, pointer :: f
    f => this%n
  end function f
end module

program test
  use class_88_m
  implicit none
  type(t), target :: xt
  class(t), pointer :: xp
  integer, pointer :: result_ptr

  ! Initialize
  xt%n = 42
  xp => xt

  ! Test type-bound procedure call with type variable
  result_ptr => xt%tbp()
  print *, result_ptr, xt%tbp()
  if (result_ptr /= 42) error stop "Failed: xt%tbp() should return 42"

  ! Test type-bound procedure call with class pointer
  result_ptr => xp%tbp()
  print *, result_ptr, xp%tbp()
  if (result_ptr /= 42) error stop "Failed: xp%tbp() should return 42"

  ! Test assignment through pointer-returning TBP with type variable
  xt%tbp() = 99
  print *, xt%tbp()
  if (xt%n /= 99) error stop "Failed: xt%tbp() = 99 should set xt%n to 99"

  ! Test assignment through pointer-returning TBP with class pointer
  xp%tbp() = 123
  print *, xp%tbp()
  if (xt%n /= 123) error stop "Failed: xp%tbp() = 123 should set xt%n to 123"

  print *, "OK"
end program test
