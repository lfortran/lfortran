module mwe
  implicit none
  type :: action
    procedure(), pointer, nopass :: a => null()
  end type
contains
  function set(this) result(p)
    type(action), target :: this
    type(action), pointer :: p
    p => this
  end function

  function get(this) result(p)
    type(action), intent(in), pointer :: this
    procedure(), pointer :: p
    p => this%a
  end function
end module

program test
  use mwe
  implicit none
  type(action), target :: say_hello
  procedure(), pointer :: p_hello
  logical :: called

  called = .false.
  set(say_hello) = action(hello)
  p_hello => get(say_hello)
  if (associated(p_hello)) call p_hello
  if (.not. called) error stop
contains
  subroutine hello
    called = .true.
  end subroutine
end program
