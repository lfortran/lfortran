module separate_compilation_55a
  implicit none
  type :: vector_t
    integer :: x(3)
    real :: r(3)
    real(8) :: d(2)
  end type
contains
  subroutine transform_index(a, result)
    type(vector_t), intent(in) :: a
    integer, intent(out) :: result
    result = dot_product(a%x(:), a%x)
  end subroutine
  real function rdot(a)
    type(vector_t), intent(in) :: a
    rdot = dot_product(a%r(:), a%r)
  end function
  real(8) function mixed(a)
    type(vector_t), intent(in) :: a
    mixed = dot_product(a%x(2:3), a%d(:))
  end function
end module
