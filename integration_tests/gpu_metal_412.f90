module gpu_metal_412_m
  ! A derived-type value that device code creates on the device heap,
  ! stored into an element of an array the host handed over, is copied out
  ! after the launch: the host gives the element's allocatable component the
  ! bounds the value has, replacing what it had, and the elements.
  implicit none
  type :: tensor_t
    integer :: n = 0
    real, allocatable :: values_(:)
  end type
  interface tensor_t
    pure module function construct(values) result(tensor)
      real, intent(in) :: values(:)
      type(tensor_t) :: tensor
    end function
  end interface
contains
  ! The size of the result is a count the function works out.
  pure function large(x) result(t)
    real, intent(in) :: x(:)
    type(tensor_t) :: t
    integer :: c, j
    c = 0
    do j = 1, size(x)
      if (x(j) > 0.5) c = c + 1
    end do
    t%values_ = x(1:c)
    t%n = c
  end function
end module

submodule (gpu_metal_412_m) gpu_metal_412_s
  implicit none
contains
  module procedure construct
    tensor%values_ = values
  end procedure
end submodule

program gpu_metal_412
  use gpu_metal_412_m
  implicit none
  integer, parameter :: n = 6
  real :: x(10, n)
  type(tensor_t), allocatable :: outputs(:), more(:)
  integer :: i, k, c
  do i = 1, n
    do k = 1, 10
      x(k, i) = mod(k * 7 + i * 3, 11) / 10.0
    end do
  end do
  allocate(outputs(n))
  ! Already allocated, with another size and lower bound.
  allocate(outputs(2)%values_(-3:20))
  outputs(2)%values_ = 7
  outputs(5)%values_ = [1.0]
  do concurrent (i = 1:n)
    outputs(i) = large(x(:, i))
  end do
  ! The constructor, in a submodule, called by the loop.
  allocate(more(n))
  do concurrent (i = 1:n)
    block
      integer :: c, j
      c = 0
      do j = 1, 10
        if (x(j, i) < 0.5) c = c + 1
      end do
      more(i) = tensor_t(x(1:c, i))
    end block
  end do
  do i = 1, n
    c = count(x(:, i) > 0.5)
    if (.not. allocated(outputs(i)%values_)) error stop 1
    if (outputs(i)%n /= c) error stop 2
    if (size(outputs(i)%values_) /= c) error stop 3
    if (lbound(outputs(i)%values_, 1) /= 1) error stop 4
    if (any(outputs(i)%values_ /= x(1:c, i))) error stop 5
    c = count(x(:, i) < 0.5)
    if (size(more(i)%values_) /= c) error stop 6
    if (any(more(i)%values_ /= x(1:c, i))) error stop 7
  end do
  print *, (size(outputs(i)%values_), i = 1, n)
end program
