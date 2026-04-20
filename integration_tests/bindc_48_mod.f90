module bindc_48_mod
  use iso_c_binding, only: c_int
  implicit none
  integer(c_int), bind(c) :: a_bindc
  integer :: b_regular
end module
