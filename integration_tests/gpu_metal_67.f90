module gpu_metal_67_mod_a
  implicit none
  type :: type_a
    real :: val
  end type
  interface type_a
    module procedure my_construct
  end interface
contains
  pure function my_construct(v) result(res)
    real, intent(in) :: v
    type(type_a) res
    res%val = v
  end function
end module

module gpu_metal_67_mod_b
  implicit none
  type :: type_b
    real :: val
  end type
  interface type_b
    module procedure my_construct
  end interface
contains
  pure function my_construct(v) result(res)
    real, intent(in) :: v
    type(type_b) res
    res%val = v * 10.0
  end function
end module

program gpu_metal_67
  use gpu_metal_67_mod_a, only : type_a
  use gpu_metal_67_mod_b, only : type_b
  implicit none
  type(type_a) :: a_arr(4)
  type(type_b) :: b_arr(4)
  integer :: i
  do concurrent (i = 1:4)
    a_arr(i) = type_a(real(i))
    b_arr(i) = type_b(real(i))
  end do
  if (abs(a_arr(1)%val - 1.0) > 1.0e-6) error stop
  if (abs(a_arr(2)%val - 2.0) > 1.0e-6) error stop
  if (abs(a_arr(3)%val - 3.0) > 1.0e-6) error stop
  if (abs(a_arr(4)%val - 4.0) > 1.0e-6) error stop
  if (abs(b_arr(1)%val - 10.0) > 1.0e-6) error stop
  if (abs(b_arr(2)%val - 20.0) > 1.0e-6) error stop
  if (abs(b_arr(3)%val - 30.0) > 1.0e-6) error stop
  if (abs(b_arr(4)%val - 40.0) > 1.0e-6) error stop
  print *, "ok"
end program
