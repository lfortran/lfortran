module gpu_metal_445_m
  ! Two kernels, each launched many times with arrays whose sizes grow and
  ! shrink from one launch to the next. The GPU runtime keeps the device
  ! buffers of a kernel from one launch to the next, so every launch has to
  ! see the current host data, write back exactly the elements of its
  ! arguments, and never see the data of the other kernel.
  implicit none
contains
  subroutine shift(x, y, n, c)
    integer, intent(in) :: n
    real, intent(in) :: x(n), c
    real, intent(inout) :: y(n)
    integer :: i
    do concurrent (i = 1:n)
      y(i) = x(i) + c
    end do
  end subroutine
  subroutine scale(z, f)
    real, intent(inout) :: z(:)
    real, intent(in) :: f
    integer :: i
    do concurrent (i = 1:size(z))
      z(i) = z(i) * f
    end do
  end subroutine
end module

program gpu_metal_445
  use gpu_metal_445_m
  implicit none
  integer, parameter :: sizes(8) = [5, 1000, 3, 700, 1, 1000, 64, 999]
  real :: x(1000), y(1010)
  real, allocatable :: z(:)
  integer :: i, m, n, r
  do r = 1, 40
    n = sizes(mod(r - 1, size(sizes)) + 1)
    x(:n) = [(real(i + r), i = 1, n)]
    y = -1
    call shift(x, y, n, real(r))
    if (any(y(:n) /= x(:n) + r)) error stop 1
    if (any(y(n+1:) /= -1)) error stop 2
    if (any(x(:n) /= [(real(i + r), i = 1, n)])) error stop 3
    m = sizes(mod(r, size(sizes)) + 1)
    if (allocated(z)) deallocate(z)
    allocate(z(m))
    z = [(real(i - r), i = 1, m)]
    call scale(z, 2.)
    if (any(z /= [(2 * real(i - r), i = 1, m)])) error stop 4
    call shift(z, y, m, 0.5)
    if (any(y(:m) /= z + 0.5)) error stop 5
    if (any(y(m+1:n) /= x(m+1:n) + r)) error stop 6
    if (any(y(max(n, m)+1:) /= -1)) error stop 7
  end do
  print *, "ok"
end program
