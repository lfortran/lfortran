! Test nested select type on the same member symbol
! When two select type blocks both access the same member (e.g. self%obj
! and other%obj), the inner block must not corrupt the outer block's context.
module select_type_29_mod
  implicit none
  private

  type, abstract :: base
  end type base

  type, extends(base) :: child
    integer :: val
  end type child

  public :: wrapper, make_child, set_val

  type :: wrapper
    class(base), allocatable :: obj
  contains
    procedure :: check
  end type wrapper

contains

  subroutine make_child(w)
    type(wrapper), intent(out) :: w
    allocate(child :: w%obj)
  end subroutine

  subroutine set_val(w, v)
    type(wrapper), intent(inout) :: w
    integer, intent(in) :: v
    select type(o => w%obj)
    type is (child)
      o%val = v
    end select
  end subroutine

  subroutine check(self, other, expected)
    class(wrapper), intent(inout) :: self
    class(wrapper), intent(in)    :: other
    integer, intent(in) :: expected
    integer :: found

    found = 0
    select type(a => self%obj)
    type is(child)

      select type(b => other%obj)
      type is(child)
        found = a%val + b%val
      end select

    end select
    if (found /= expected) error stop

  end subroutine check

end module select_type_29_mod


program select_type_29
  use select_type_29_mod
  implicit none

  type(wrapper) :: w1, w2

  call make_child(w1)
  call make_child(w2)
  call set_val(w1, 10)
  call set_val(w2, 20)

  call w1%check(w2, 30)
  print *, "ok"
end program select_type_29
