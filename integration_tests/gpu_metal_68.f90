module gpu_metal_68_m
  implicit none
  type tensor_t
    real :: v
  contains
    procedure :: values => get_values
  end type
contains
  pure function get_values(self) result(v)
    class(tensor_t), intent(in) :: self
    real :: v
    v = self%v
  end function
end module

program main
  use gpu_metal_68_m, only : tensor_t
  implicit none
  type(tensor_t) :: inp
  real :: res(1)
  integer :: i
  inp%v = 3.14
  res = 0.0
  do concurrent(i=1:1)
    res(i) = xfn(inp)
  end do
  if (abs(res(1) - 3.14) > 1.0e-5) error stop
  print *, "ok"
contains
  pure function xfn(inp) result(outp)
    type(tensor_t), intent(in) :: inp
    real :: outp
    associate(s => inp%values())
      outp = s
    end associate
  end function
end program
