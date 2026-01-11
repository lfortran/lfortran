module derived_types_86_mod
type :: json_ser_config
  integer :: l1 = 14
  character(len=:), allocatable :: indent
end type
type :: json_serializer
  type(json_ser_config) :: config = json_ser_config()
end type
end module

program derived_types_86
  use derived_types_86_mod
  type(json_serializer) :: ser
  ser = json_serializer()
  ser%config%indent = "Hello"
  if (ser%config%l1 /= 14) error stop
end program derived_types_86