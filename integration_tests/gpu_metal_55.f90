module gpu_metal_55_mod
  implicit none
  interface
    module subroutine learn(x)
      implicit none
      real, intent(inout) :: x(:)
    end subroutine
  end interface
end module

submodule(gpu_metal_55_mod) gpu_metal_55_s
  implicit none
contains
  module procedure learn
    integer :: i
    associate(n => x)
      block
        real :: r(size(n))
        do concurrent (i = 1:3)
          r(i) = real(i) * 2.0
        end do
        do concurrent (i = 1:3)
          n(i) = r(i) + 1.0
        end do
      end block
    end associate
  end procedure learn
end submodule gpu_metal_55_s

program gpu_metal_55
  use gpu_metal_55_mod
  implicit none
  real :: arr(5)
  integer :: i
  arr = 0.0
  call learn(arr)
  if (abs(arr(1) - 3.0) > 1.0e-6) error stop
  if (abs(arr(2) - 5.0) > 1.0e-6) error stop
  if (abs(arr(3) - 7.0) > 1.0e-6) error stop
  if (abs(arr(4) - 0.0) > 1.0e-6) error stop
  if (abs(arr(5) - 0.0) > 1.0e-6) error stop
  print *, "PASS"
end program

