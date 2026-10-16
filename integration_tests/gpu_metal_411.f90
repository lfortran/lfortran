module gpu_metal_411_m
  ! A derived-type value that device code creates, whose allocatable
  ! component gets a size the iteration works out from the values it reads,
  ! is on the device heap. The constructor is in a submodule.
  implicit none
  type :: tensor_t
    real, allocatable :: values_(:)
  end type
  interface tensor_t
    pure module function construct(values) result(tensor)
      real, intent(in) :: values(:)
      type(tensor_t) :: tensor
    end function
  end interface
contains
  pure real function total(t)
    type(tensor_t), intent(in) :: t
    total = sum(t%values_) + size(t%values_)
  end function
end module

submodule (gpu_metal_411_m) gpu_metal_411_s
  implicit none
contains
  module procedure construct
    tensor%values_ = values
  end procedure
end submodule

program gpu_metal_411
  use gpu_metal_411_m
  implicit none
  integer, parameter :: n = 6
  real :: x(10, n), r(n), s(n)
  integer :: i, k, c
  do i = 1, n
    do k = 1, 10
      x(k, i) = mod(k * 7 + i * 3, 11) / 10.0
    end do
  end do
  do concurrent (i = 1:n)
    block
      type(tensor_t) :: t, u
      integer :: c, j
      c = 0
      do j = 1, 10
        if (x(j, i) > 0.5) c = c + 1
      end do
      t = tensor_t(x(1:c, i))
      u = t
      u%values_(1) = -1
      r(i) = total(t) + t%values_(c)
      s(i) = total(u)
    end block
  end do
  do i = 1, n
    c = count(x(:, i) > 0.5)
    if (abs(r(i) - (sum(x(1:c, i)) + c + x(c, i))) > 1e-4) error stop 1
    if (abs(s(i) - (sum(x(2:c, i)) - 1 + c)) > 1e-4) error stop 2
  end do
  print *, r
  print *, s
end program
