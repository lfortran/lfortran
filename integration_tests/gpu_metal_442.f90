module gpu_metal_442_m
  ! A separate module procedure implemented in a submodule, called from a
  ! routine of its module that the loop calls, runs on the device with the
  ! body the submodule gives it (#14649).
  implicit none
  interface
    pure module function twice(x) result(y)
      real, intent(in) :: x
      real :: y
    end function
  end interface
contains
  pure function g(x) result(y)
    real, intent(in) :: x
    real :: y
    y = twice(x) + 1
  end function
end module

submodule (gpu_metal_442_m) gpu_metal_442_s
  implicit none
contains
  module procedure twice
    y = 2*x
  end procedure
end submodule

program gpu_metal_442
  use gpu_metal_442_m
  implicit none
  real :: x(4), y(4)
  integer :: i
  x = [1., 2., 3., 4.]
  do concurrent (i = 1:4)
    y(i) = g(x(i))
  end do
  if (any(y /= [3., 5., 7., 9.])) error stop 1
  print *, "ok"
end program
