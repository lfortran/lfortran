module select_type_10_mod
  implicit none

  type, abstract :: base
  end type base

  type, extends(base) :: int_val
    integer :: key
  end type int_val

contains
   subroutine check(self)
    class(int_val), intent(inout) :: self
    self%key = 10
  end subroutine check

  subroutine check2(self)
    type(int_val), intent(inout) :: self
    self%key = 20
  end subroutine check2
end module select_type_10_mod

program select_type_10
  use select_type_10_mod
  implicit none

  class(base), pointer :: val

  allocate(int_val :: val)
  select type(val)
  type is(int_val)
      call check(val)
      if (val%key /= 10) error stop
      call check2(val)
      if (val%key /= 20) error stop
  class default
    error stop
  end select
end program select_type_10