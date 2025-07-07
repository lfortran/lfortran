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

  type :: temp
    class(base), allocatable :: bas
  end type temp

contains

  subroutine print_value(val, x)
    class(base), intent(inout) :: val
    class(real_val), intent(out) :: x
    select type(v => val)
    type is(int_val)
      v%i = 50
      x%r = 3.5
    end select
  end subroutine print_value

  subroutine print_value2(val)
    class(temp), intent(inout) :: val
    select type(v => val%bas)
    type is(int_val)
       v%i = 50
    end select
  end subroutine print_value2

end module select_type_09_mod

program select_type_09
  use select_type_09_mod
  implicit none

  class(base), allocatable :: val
  class(real_val), allocatable :: r_val
  type(temp), allocatable :: val2

  allocate(int_val :: val)
  allocate(r_val)
  select type(val)
  type is(int_val)
    val%i = 42
    call print_value(val, r_val)
    if (val%i /= 50) error stop
  end select
  if (r_val%r /= 3.5) error stop

  allocate(val2)
  allocate(int_val :: val2%bas)
  call print_value2(val2)

  select type(v => val2%bas)
  type is(int_val)
     if (v%i /= 50) error stop
  end select
end program select_type_09
