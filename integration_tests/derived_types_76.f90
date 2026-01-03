module derived_type_76_mod
  implicit none
  type, abstract :: toml_value
    integer :: origin = -1
  end type 
  type, extends(toml_value) :: toml_table 
    logical :: l1 = .true.
  end type 
  type :: parser
     type(toml_table), allocatable :: s1
  end type
end module

program derived_type_76
use derived_type_76_mod
  type(parser) :: v1
  allocate(v1%s1)
  v1%s1 = toml_table()
  if (v1%s1%origin /= -1) error stop
  if (.not. v1%s1%l1) error stop
end program
