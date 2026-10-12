module gpu_metal_416_m
  ! The first loop of the FIATS training tests, with the tensor a heap
  ! type (the second loop makes an array of tensors on the device): a tensor
  ! stored into an element of an argument, read back in the same iteration
  ! through a type-bound function whose result is declared with the size of
  ! the tensor's component, and a tensor made from that.
  implicit none
  type tensor_t
    real, allocatable :: values_(:)
  contains
    procedure :: values
  end type
  interface tensor_t
    pure module function construct(values) result(tensor)
      implicit none
      real, intent(in) :: values(:)
      type(tensor_t) tensor
    end function
  end interface
  interface
    pure module function values(self) result(tensor_values)
      implicit none
      class(tensor_t), intent(in) :: self
      real, allocatable :: tensor_values(:)
    end function
  end interface
end module

submodule (gpu_metal_416_m) gpu_metal_416_s
  implicit none
contains
  module procedure construct
    tensor%values_ = values
  end procedure
  module procedure values
    tensor_values = self%values_
  end procedure
end submodule

program gpu_metal_416
  use gpu_metal_416_m
  implicit none
  integer, parameter :: nb = 2, ni = 6
  type(tensor_t), allocatable :: ins(:, :), outs(:, :)
  real :: harvest(2, nb, ni)
  integer :: b, it
  do it = 1, ni
    do b = 1, nb
      harvest(:, b, it) = [mod(b + it, 3), mod(b * it, 4)] / 4.0
    end do
  end do
  allocate(ins(nb, ni), outs(nb, ni))
  do concurrent (b = 1:nb, it = 1:ni)
    ins(b, it) = tensor_t(merge(1., 0., harvest(:, b, it) < 0.5))
    outs(b, it) = either(ins(b, it))
  end do
  do it = 1, ni
    do b = 1, nb
      if (any(ins(b, it)%values_ /= merge(1., 0., harvest(:, b, it) < 0.5))) &
        error stop 1
      if (size(outs(b, it)%values_) /= 1) error stop 2
      if (outs(b, it)%values_(1) /= &
          merge(1., 0., sum(ins(b, it)%values_) > 0.99)) error stop 3
    end do
  end do
  ! An array of tensors the device creates.
  do concurrent (it = 1:ni)
    block
      type(tensor_t) :: two(2)
      two(1) = ins(1, it)
      two(2) = outs(2, it)
      outs(1, it) = tensor_t([two(1)%values(), two(2)%values()])
    end block
  end do
  do it = 1, ni
    if (size(outs(1, it)%values_) /= 3) error stop 4
    if (any(outs(1, it)%values_ /= [ins(1, it)%values_, &
        outs(2, it)%values_])) error stop 5
  end do
  print *, "ok"
contains
  pure function either(inputs) result(expected_outputs)
    type(tensor_t), intent(in) :: inputs
    type(tensor_t) expected_outputs
    expected_outputs = tensor_t([merge(1., 0., sum(inputs%values()) > 0.99)])
  end function
end program
