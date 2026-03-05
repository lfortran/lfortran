module kind_parameters_m
  implicit none
  integer, parameter :: double_precision = kind(1D0)
end module kind_parameters_m


module tensor_m
  use kind_parameters_m, only : double_precision
  implicit none

  type tensor_t(k)
    integer, kind :: k = kind(1.)
    real(k), allocatable :: values_(:)
  end type tensor_t

  interface
    pure module function double_precision_values(self) result(tensor_values)
      implicit none
      class(tensor_t(double_precision)), intent(in) :: self
      double precision, allocatable :: tensor_values(:)
    end function
  end interface

contains

  pure module function double_precision_values(self) result(tensor_values)
    class(tensor_t(double_precision)), intent(in) :: self
    double precision, allocatable :: tensor_values(:)

    allocate(tensor_values(size(self%values_)))
    tensor_values = self%values_
  end function double_precision_values

end module tensor_m


program main
  use tensor_m
  use kind_parameters_m, only: double_precision
  implicit none

  type(tensor_t(double_precision)) :: t
  double precision, allocatable :: vals(:)

  allocate(t%values_(3))
  t%values_ = [1.0d0, 2.0d0, 3.0d0]

  vals = double_precision_values(t)

  print *, "Tensor values:"
  print *, vals

  if (any(vals /= [1.0d0,2.0d0,3.0d0])) error stop

end program main
