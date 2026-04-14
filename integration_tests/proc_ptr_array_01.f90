module proc_ptr_array_01_mod
  implicit none

  type :: op_type
    procedure(op_iface), pass(this), pointer :: compute => null()
  end type

  abstract interface
    subroutine op_iface(this, a, b)
      import op_type
      class(op_type), intent(in) :: this
      real, dimension(:,:), intent(in) :: a
      real, dimension(:,:), intent(out) :: b
    end subroutine
  end interface

contains

  subroutine double_it(this, a, b)
    class(op_type), intent(in) :: this
    real, dimension(:,:), intent(in) :: a
    real, dimension(:,:), intent(out) :: b
    b = a * 2.0
  end subroutine

  subroutine run_op(obj, x, y)
    type(op_type), intent(inout) :: obj
    real, dimension(:,:), intent(in) :: x
    real, dimension(:,:), intent(out) :: y
    call obj%compute(x, y)
  end subroutine

end module proc_ptr_array_01_mod

program proc_ptr_array_01
  use proc_ptr_array_01_mod
  implicit none
  type(op_type) :: obj
  real :: x(2,2), y(2,2)

  x = 3.0
  y = 0.0

  obj%compute => double_it
  call run_op(obj, x, y)

  if (abs(y(1,1) - 6.0) > 1e-5) error stop
  if (abs(y(2,2) - 6.0) > 1e-5) error stop
  print *, "PASS"
end program
