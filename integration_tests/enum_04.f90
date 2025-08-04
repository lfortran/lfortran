module enum_04_mod
  use iso_c_binding
  implicit none

  enum, bind(c)
    enumerator :: red = 1, green = 2, blue = 3
  end enum

end module enum_04_mod

program enum_04
  use iso_c_binding
  use enum_04_mod
  implicit none

  integer(c_int) :: c

  c = red
  if (c /= 1) error stop
  c = green
  if (c /= 2) error stop
  c = blue
  if (c /= 3) error stop
end program enum_04
