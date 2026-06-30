module bindc_c_main_mod
  use iso_c_binding, only: c_int
  implicit none
contains
  subroutine bump_value(val) bind(c, name="bump_value")
    integer(c_int), intent(inout) :: val
    if (val /= 41) error stop
    val = val + 1
  end subroutine bump_value
end module
