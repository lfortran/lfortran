module bindc_48_mod
  use iso_c_binding, only: c_int
  implicit none
  integer(c_int), bind(c) :: a_bindc
  integer :: b_regular
  integer :: c_regular
  integer(c_int), bind(c) :: d_bindc
  integer :: e_regular
end module
