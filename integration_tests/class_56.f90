module class_56_mod1
  implicit none
  type :: toml_value
    integer :: x = 0
  contains 
    procedure :: accept
  end type

contains

  subroutine accept(self)
    class(toml_value), intent(inout) :: self
    self%x = self%x + 1
  end subroutine

end module

module class_56_mod2
  use class_56_mod1
  implicit none

  type :: ser_config
  contains 
    procedure :: temp
    procedure :: temp2
  end type

contains

  subroutine temp(self, val)
    class(ser_config), intent(inout) :: self
    class(toml_value), intent(inout) :: val

    select type(val)
    type is (toml_value)
      call val%accept()
    class default
      print *, "Unknown type"
    end select
  end subroutine

  subroutine temp2(self, val)
    class(ser_config), intent(inout) :: self
    class(toml_value), intent(inout) :: val

    select type(val)
    class is (toml_value)
      call val%accept()
    class default
      print *, "Unknown type"
    end select
  end subroutine

end module

program class_56
  use class_56_mod2
  implicit none

  type(ser_config) :: cfg
  type(toml_value) :: v

  v%x = 1
  call cfg%temp(v)
  if (v%x /= 2) error stop
  call cfg%temp2(v)
  if (v%x /= 3) error stop
end program
