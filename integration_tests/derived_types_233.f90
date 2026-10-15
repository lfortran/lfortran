module derived_types_233_m
  ! An array of a derived type with an allocatable component, returned by
  ! a function and assigned whole with --realloc-lhs-arrays, is copied
  ! element by element. Copying an element must not take stack memory that
  ! is only freed when the procedure returns: with a million elements, that
  ! overflows the stack.
  implicit none
  type :: tensor_t
    real, allocatable :: values_(:)
  end type
end module

program derived_types_233
  use derived_types_233_m
  implicit none
  integer, parameter :: n = 1250000
  type(tensor_t), allocatable :: inputs(:)
  inputs = make()
  if (size(inputs) /= n) error stop 1
  if (any(inputs(7)%values_ /= [7., 2.])) error stop 2
  if (any(inputs(n)%values_ /= [n*1., 2.])) error stop 3
  print *, "ok"
contains
  function make()
    type(tensor_t), allocatable :: make(:)
    integer :: i
    allocate(make(n))
    do i = 1, n
      allocate(make(i)%values_(2))
      make(i)%values_ = [real(i), 2.]
    end do
  end function
end program
