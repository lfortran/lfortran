program bindc_46
  implicit none

  type, bind(c) :: t
    integer :: x
  end type

  type :: container
    type(t) :: field
  end type

  abstract interface
    subroutine iface(a) bind(C)
      import :: t
      type(t), value, intent(in) :: a
    end subroutine
  end interface

  procedure(iface), pointer :: f
  type(t) :: v
  type(container) :: obj

  ! Test 1: direct call with local variable
  v%x = 42
  call sub(v)

  ! Test 2: call through procedure pointer
  f => sub
  v%x = 99
  call f(v)

  ! Test 3: pass struct member
  obj%field%x = 7
  call sub(obj%field)

  ! Test 4: struct member through procedure pointer
  obj%field%x = 123
  call f(obj%field)

  print *, "ok"

contains
  subroutine sub(a) bind(C)
    type(t), value, intent(in) :: a
    select case (a%x)
    case (42)
      if (a%x /= 42) error stop
    case (99)
      if (a%x /= 99) error stop
    case (7)
      if (a%x /= 7) error stop
    case (123)
      if (a%x /= 123) error stop
    case default
      error stop
    end select
  end subroutine
end program
