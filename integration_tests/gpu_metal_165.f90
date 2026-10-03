module gpu_metal_165_m
  implicit none
  type :: tensor_t
    real :: v
  end type
  interface tensor_t
    module procedure construct
  end interface
contains
  pure function construct(x) result(tensor)
    real, intent(in) :: x
    type(tensor_t) :: tensor
    tensor%v = x
  end function
end module

program gpu_metal_165
  use gpu_metal_165_m, only : tensor_t
  implicit none
  type(tensor_t) :: inputs(2), outputs(2)
  integer :: i
  inputs(1) = tensor_t(1.0)
  inputs(2) = tensor_t(2.0)
  do concurrent(i = 1:2)
    outputs(i) = do_map(inputs(i))
  end do
  if (abs(outputs(1)%v - 2.0) > 1e-6) error stop
  if (abs(outputs(2)%v - 4.0) > 1e-6) error stop
  print *, "ok"
contains
  pure function do_map(tensor) result(mapped)
    type(tensor_t), intent(in) :: tensor
    type(tensor_t) :: mapped
    associate(v => tensor%v)
      associate(nv => v * 2.0)
        mapped = tensor_t(nv)
      end associate
    end associate
  end function
end program
