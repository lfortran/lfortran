module gpu_metal_398_m
  ! A device routine that binds an associate name to the allocatable array
  ! result of a type-bound function, as `associate (v => t%values())`. The
  ! shared lowering gives the result, the associate name and the copies
  ! between them temporaries that are allocated, reallocated or moved, each
  ! sized from the one before. A device has no heap, so each becomes an
  ! array of the shape it has on entry to the routine, and an argument the
  ! kernel gives the routine, sized on the host per iteration.
  implicit none
  type :: tensor_t
    real, allocatable :: values_(:)
  contains
    procedure :: values
  end type
contains
  pure function values(self) result(v)
    class(tensor_t), intent(in) :: self
    real, allocatable :: v(:)
    v = self%values_
  end function

  pure function total(t) result(r)
    type(tensor_t), intent(in) :: t
    real :: r
    associate (v => t%values())
      r = sum(v) + size(v)
    end associate
  end function
end module

program gpu_metal_398
  use gpu_metal_398_m
  implicit none
  type(tensor_t) :: x(4)
  real :: y(4)
  integer :: i, j, m(4)
  m = [2, 5, 1, 3]
  do i = 1, 4
    allocate(x(i)%values_(m(i)))
    x(i)%values_ = [(real(j*i), j = 1, m(i))]
  end do
  do concurrent (i = 1:4)
    y(i) = total(x(i))
  end do
  do i = 1, 4
    if (abs(y(i) - (i*m(i)*(m(i) + 1)/2 + m(i))) > 1e-4) error stop 1
  end do
  print *, "ok"
end program
