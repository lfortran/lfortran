module select_type_09_mod
  implicit none

  type, abstract :: base
  end type base

  type, extends(base) :: int_val
    integer :: i
  end type int_val

  type, extends(base) :: real_val
    real :: r
  end type real_val

contains

  subroutine print_value(val, x)
    class(base), intent(in) :: val
    class(real_val), intent(out) :: x
    select type(v => val)
    type is(int_val)
      v%i = 50
      x%r = 3.5
    end select
  end subroutine print_value

end module select_type_09_mod

program select_type_09
  use select_type_09_mod
  implicit none

  class(base), allocatable :: val
  class(real_val), allocatable :: r_val

  allocate(int_val :: val)
  allocate(r_val)
  select type(val)
  type is(int_val)
    val%i = 42
    call print_value(val, r_val)
    if (val%i /= 50) error stop
  end select
  if (r_val%r /= 3.5) error stop
end program select_type_09
