module m
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
end module m

program test
  use m
  implicit none
  type(t), target :: xt
  class(t), pointer :: xp
  integer, pointer :: result_ptr

  ! Initialize
  xt%n = 42
  xp => xt

  ! Test type-bound procedure call with type variable
  result_ptr => xt%tbp()
  if (result_ptr /= 42) error stop "Failed: xt%tbp() should return 42"

  ! Test type-bound procedure call with class pointer
  result_ptr => xp%tbp()
  if (result_ptr /= 42) error stop "Failed: xp%tbp() should return 42"

  print *, "OK"
end program test
