module gpu_metal_462_m
  implicit none
  type :: buf_t
    real, allocatable :: v(:)
  end type
contains
  subroutine fill(x, c)
    real, intent(inout) :: x(:)
    real, intent(in) :: c
    integer :: i
    do concurrent (i = 1:size(x))
      x(i) = x(i) + c
    end do
  end subroutine
end module

program gpu_metal_462
  ! Arrays a kernel writes that the host then reallocates, deallocates and
  ! allocates again, at another size or at the same address, in a loop. The
  ! device keeps what a kernel writes until the host waits, so none of this
  ! may hand a later kernel the data of an array that is gone.
  use gpu_metal_462_m
  implicit none
  real, allocatable :: x(:), y(:)
  type(buf_t) :: t
  integer :: i, k, n
  x = [real :: 1, 2, 3]
  do k = 1, 30
    do concurrent (i = 1:size(x))
      x(i) = x(i) * 2
    end do
    ! An assignment that reallocates what the kernel wrote.
    x = [x, real(k)]
    if (size(x) /= 3 + k) error stop 1
  end do
  if (abs(x(1) - 2.0**30) > 1) error stop 2
  if (abs(x(size(x)) - 30) > 1e-3) error stop 3

  do k = 1, 40
    n = mod(k, 5) + 1
    allocate(y(n))
    y = 1
    do concurrent (i = 1:n)
      y(i) = y(i) + i
    end do
    if (k == 40) then
      if (any(abs(y - [(real(i + 1), i = 1, n)]) > 1e-3)) error stop 4
    end if
    deallocate(y)
  end do

  ! An associate name for a component, written by kernels and read by the
  ! host through the component.
  allocate(t%v(10))
  t%v = 0
  associate (w => t%v)
    do k = 1, 5
      do concurrent (i = 1:size(w))
        w(i) = w(i) + 1
      end do
      call fill(t%v, 1.0)
    end do
  end associate
  if (any(abs(t%v - 10) > 1e-3)) error stop 5
  print *, "ok"
end program
