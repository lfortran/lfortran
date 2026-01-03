module class_56_mod1
  implicit none
  type :: toml_value
    integer :: x = 0
  contains 
    procedure :: accept
  end type

  type :: string_t
    character(len=:), allocatable :: s
  end type string_t

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

  subroutine pkgcfg_list_all(descriptions)  
    type(string_t), optional, allocatable, intent(out) :: descriptions(:)
    allocate(descriptions(2))
    descriptions(1)%s = "Package 1"
    descriptions(2)%s = "Package 2"
  end subroutine

end module

program class_56
  use class_56_mod2
  implicit none

  type(ser_config) :: cfg
  type(toml_value) :: v
  type(string_t), allocatable :: descriptions(:)

  v%x = 1
  call cfg%temp(v)
  if (v%x /= 2) error stop
  call cfg%temp2(v)
  if (v%x /= 3) error stop
  call pkgcfg_list_all(descriptions)
  if (descriptions(1)%s /= "Package 1") error stop
  if (descriptions(2)%s /= "Package 2") error stop
end program
