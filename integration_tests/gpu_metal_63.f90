module gpu_metal_63_mod
  implicit none
  type :: mytype_t
    real :: val
  end type

  interface mytype_t
    module procedure construct
  end interface

contains

  pure function construct(v) result(t)
    real, intent(in) :: v
    type(mytype_t) :: t
    t%val = v
  end function
end module

program gpu_metal_63
  use gpu_metal_63_mod
  implicit none
  type(mytype_t) :: arr(4)
  integer :: i

  do concurrent (i = 1:4)
    arr(i) = mytype_t(real(i))
  end do

  print *, arr(1)%val
  print *, arr(2)%val
  print *, arr(3)%val
  print *, arr(4)%val

  if (abs(arr(1)%val - 1.0) > 1.0e-6) error stop
  if (abs(arr(2)%val - 2.0) > 1.0e-6) error stop
  if (abs(arr(3)%val - 3.0) > 1.0e-6) error stop
  if (abs(arr(4)%val - 4.0) > 1.0e-6) error stop
  print *, "PASS"
end program
