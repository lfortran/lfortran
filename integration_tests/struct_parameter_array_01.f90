program struct_parameter_array_01
  implicit none

  type :: item
    character(1) :: tag
  end type

  type(item), parameter :: items(2) = [item("a"), item("b")]
  character(1), parameter :: tags(*) = items%tag

  print *, tags

  if (tags(1) /= "a" .or. tags(2) /= "b") error stop
end program struct_parameter_array_01
