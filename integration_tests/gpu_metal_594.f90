! A routine that the loop calls has an ASSOCIATE inside another one, which
! calls a procedure of a module.
module gpu_metal_594_m
  implicit none
  type :: tensor_t
    real :: v
  end type
contains
  pure function twice(x) result(r)
    real, intent(in) :: x
    real :: r
    r = 2 * x
  end function
end module

program gpu_metal_594
  use gpu_metal_594_m, only: tensor_t, twice
  implicit none
  type(tensor_t) :: inputs(2), outputs(2)
  integer :: i
  inputs(1) = tensor_t(1.0)
  inputs(2) = tensor_t(2.0)
  do concurrent(i = 1:2)
    outputs(i) = do_map(inputs(i))
  end do
  print *, outputs(1)%v, outputs(2)%v
  if (abs(outputs(1)%v - 2.0) > 1e-6) error stop 1
  if (abs(outputs(2)%v - 4.0) > 1e-6) error stop 2
contains
  pure function do_map(tensor) result(mapped)
    type(tensor_t), intent(in) :: tensor
    type(tensor_t) :: mapped
    associate(v => tensor%v)
      associate(w => v + 0.0)
        mapped%v = twice(w)
      end associate
    end associate
  end function
end program
