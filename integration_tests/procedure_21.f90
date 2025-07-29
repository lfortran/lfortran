module procedure_21_mod
  implicit none
  private
  public :: is_valid

  interface is_valid
    module procedure is_valid_scalar
    module procedure is_valid_array
  end interface

contains

  integer function is_valid_array(arr)
    integer, intent(in) :: arr(:)
    is_valid_array = count(arr > 0)
  end function

  elemental integer function is_valid_scalar(x)
    integer, intent(in) :: x
    is_valid_scalar = -1
  end function

end module procedure_21_mod

program procedure_21
  use procedure_21_mod
  implicit none
  if (is_valid([1, 2, 3]) /= 3) error stop
  if (is_valid([1, -2, -3]) /= 1) error stop
  if (is_valid([1, -2, 3]) /= 2) error stop
end program procedure_21