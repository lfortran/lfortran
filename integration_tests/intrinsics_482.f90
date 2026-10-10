module intrinsics_482_m
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

program intrinsics_482
  ! dot_product on a section of a derived-type array component
  use intrinsics_482_m
  implicit none
  type(vector_t) :: v
  integer :: res
  v%x = [1, 2, 3]
  v%r = [1.5, 2.0, 0.5]
  v%d = [0.5d0, 2.0d0]
  call transform_index(v, res)
  print *, res, rdot(v), mixed(v)
  if (res /= 14) error stop
  if (abs(rdot(v) - 6.5) > 1e-6) error stop
  if (abs(mixed(v) - 7.0d0) > 1d-12) error stop
end program
