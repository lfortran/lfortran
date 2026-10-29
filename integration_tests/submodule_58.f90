module batch_module
  implicit none
  type tensor
    real, allocatable :: values(:)
  end type
  type batch
    type(tensor), allocatable :: tensors(:)
  end type
  interface
    pure module function make_batch(tensors) result(b)
      type(tensor), intent(in) :: tensors(:)
      type(batch) :: b
    end function
  end interface
end module

submodule(batch_module) batch_impl
contains
  module procedure make_batch
    b%tensors = tensors
  end procedure
end submodule

program main
  use batch_module
  implicit none
  type(tensor) :: input(1)
  type(batch) :: b
  integer i
  allocate(input(1)%values(1))
  input(1)%values(1) = 2.
  ! Repeated result copies must not grow the caller's stack.
  do i = 1, 500000
    b = make_batch(input)
  end do
  input(1)%values(1) = 3.
  if (.not. allocated(b%tensors)) error stop
  if (size(b%tensors) /= 1) error stop
  if (.not. allocated(b%tensors(1)%values)) error stop
  if (size(b%tensors(1)%values) /= 1) error stop
  if (b%tensors(1)%values(1) /= 2.) error stop
end program
