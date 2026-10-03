module pdt_15_m
  implicit none
  type :: base_t(k)
    integer, kind :: k = 4
    real(k), allocatable :: x(:)
  end type
  type, extends(base_t) :: child_t(j)
    integer, kind :: j = 4
  end type
end module

program pdt_15
  use pdt_15_m
  implicit none
  type(child_t) :: obj
  allocate(obj%x(3))
  obj%x = [1.0, 2.0, 3.0]
  call sub(obj)
contains
  subroutine sub(x)
    class(base_t), intent(in) :: x
    if (size(x%x) /= 3) error stop
    if (abs(x%x(1) - 1.0) > 1.0e-6) error stop
    if (abs(x%x(2) - 2.0) > 1.0e-6) error stop
    if (abs(x%x(3) - 3.0) > 1.0e-6) error stop
  end subroutine
end program
