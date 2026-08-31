program gpu_metal_198
  ! Test: a `do concurrent` body calls a function whose explicit-shape array
  ! result is sized by a derived-type component (`s%m_`).  The GPU kernel
  ! needs a workspace buffer for the result temporary, and the host has to
  ! read the extent out of the struct argument to size that buffer.
  implicit none

  type :: inner_t
    integer :: k_
  end type

  type :: t
    integer :: m_
    type(inner_t) :: in_
  end type

  type(t) :: s
  real, allocatable :: d(:), g(:)
  integer :: col

  s%m_ = 4
  s%in_%k_ = 3

  allocate(d(s%m_))
  d = 0.0
  do concurrent (col = 1:1)
    d(:) = ones(s%m_)
  end do
  if (abs(sum(d) - 4.0) > 1.0e-5) error stop "wrong result for s%m_"

  ! same, but the extent comes from a nested component
  allocate(g(s%in_%k_))
  g = 0.0
  do concurrent (col = 1:1)
    g(:) = ones(s%in_%k_)
  end do
  if (abs(sum(g) - 3.0) > 1.0e-5) error stop "wrong result for s%in_%k_"

  print *, "PASS"

contains

  pure function ones(length) result(v)
    integer, intent(in) :: length
    real :: v(length)
    integer :: i
    do i = 1, length
      v(i) = 1.0
    end do
  end function ones

end program gpu_metal_198
