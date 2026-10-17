module gpu_metal_460_m
  ! Kernels launched over and over by a host loop, with host code between
  ! them that reads and writes the arrays the kernels use. A launch does not
  ! wait for its kernel and the device keeps what a kernel writes, so the
  ! host must wait exactly where it next needs a kernel's results.
  implicit none
  integer :: calls = 0
contains
  subroutine scale(x, f)
    real, intent(inout) :: x(:)
    real, intent(in) :: f
    integer :: i
    do concurrent (i = 1:size(x))
      x(i) = x(i) * f
    end do
  end subroutine

  pure real function total(v)
    real, intent(in) :: v(:)
    total = sum(v)
  end function

  subroutine count_call()
    calls = calls + 1
  end subroutine
end module

program gpu_metal_460
  use gpu_metal_460_m
  implicit none
  integer, parameter :: n = 100, steps = 300
  real :: a(n), b(n), c(n), d(n), s, t
  integer :: i, k, last
  a = 0; b = 1; c = 0; d = 1
  s = 0
  last = 0
  do k = 1, steps
    do concurrent (i = 1:n)
      a(i) = a(i) + b(i)
    end do
    ! The host writes what the kernel above reads: the next launch sees it.
    b(1) = b(1) + 1
    ! A kernel reading what the one before it wrote.
    do concurrent (i = 1:n)
      c(i) = 2 * a(i)
    end do
    ! The host reads what a kernel wrote, now and then.
    if (mod(k, 50) == 0) then
      if (abs(a(1) - real(k * (k + 1) / 2)) > 0.5) error stop 1
      if (abs(c(n) - 2 * real(k)) > 0.5) error stop 2
    end if
    ! Calls in between: one that is not pure, and a pure one reading host
    ! data the kernels only read.
    call count_call()
    s = s + total(b(2:3))
    ! A local of a block written by a kernel and read by the next one.
    block
      real :: w(n)
      do concurrent (i = 1:n)
        w(i) = c(i) + 1
      end do
      do concurrent (i = 1:n)
        d(i) = w(i) - c(i)
      end do
    end block
    if (k == steps - 1) cycle
    last = k
    if (k > steps + 10) exit
  end do
  if (calls /= steps) error stop 3
  if (abs(s - 2 * steps) > 0.5) error stop 4
  if (last /= steps) error stop 5
  if (abs(a(1) - real(steps * (steps + 1) / 2)) > 0.5) error stop 6
  if (any(abs(a(2:) - steps) > 0.5)) error stop 7
  if (any(abs(c - 2 * a) > 0.5)) error stop 8
  if (any(abs(d - 1) > 1e-3)) error stop 9

  ! A kernel in a procedure called in a loop, and an early exit with a
  ! launch pending.
  do k = 1, 10
    call scale(d, 2.0)
    if (d(1) > 100) exit
  end do
  if (k /= 7 .or. any(abs(d - 128) > 1e-3)) error stop 10

  t = 0
  do k = 1, 20
    do concurrent (i = 1:n)
      a(i) = real(k)
    end do
    if (k == 5) exit
  end do
  t = sum(a)
  if (abs(t - 5 * n) > 0.5) error stop 11
  print *, "ok"
end program
