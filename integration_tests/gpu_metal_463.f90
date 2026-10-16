module gpu_metal_463_m
  implicit none
  type :: work_t
    real, allocatable :: g(:,:), r(:,:,:)
  end type
end module

program gpu_metal_463
  ! Whole-array assignments in a host loop that launches kernels, on the
  ! data the kernels use, run on the device as parallel loops of their own,
  ! so that the loop does not wait for the device in every iteration. Each
  ! has to compute what it computes on the host: elemental expressions,
  ! sum and product along a dimension, through associate names and
  ! components, and the assignments that must stay on the host because
  ! their target overlaps what they read or may be reallocated.
  use gpu_metal_463_m
  implicit none
  integer, parameter :: n = 6, m = 4, p = 3, steps = 50
  type(work_t) :: w
  real :: x(n), y(n), q(n, m)
  real, allocatable :: z(:)
  integer :: i, j, k, step
  allocate(w%g(n, m), w%r(n, m, p))
  x = 1; y = 0; q = 2
  z = [real :: 1, 2, 3]
  associate (g => w%g, r => w%r)
    do step = 1, steps
      g = 0.
      r = 1.
      do concurrent (i = 1:n, j = 1:m)
        do k = 1, p
          r(i, j, k) = r(i, j, k) * (i + j + k)
        end do
      end do
      g = sum(r, dim=3)
      q = product(r, dim=3) / (q * 0 + 1)
      do concurrent (i = 1:n)
        x(i) = x(i) + g(i, 1) / 1000
      end do
      ! Elemental, element for element on its own target.
      y = 2 * x + y * 0.5
      x = x * 0.5 + 1
      ! The target overlaps what the value reads otherwise: on the host.
      y = y(n:1:-1)
      x = x(1) + 0 * x
      ! An allocatable the assignment may reallocate: on the host.
      z = x(1:size(z)) + z * 0
    end do
  end associate
  do j = 1, m
    do i = 1, n
      if (abs(w%g(i, j) - real(3 * (i + j) + 6)) > 1e-3) error stop 1
      if (abs(q(i, j) - real((i + j + 1) * (i + j + 2) * (i + j + 3))) &
          > 1e-2 * q(i, j)) error stop 2
    end do
  end do
  if (any(abs(x - x(1)) > 1e-5)) error stop 3
  if (size(z) /= 3 .or. any(abs(z - x(1)) > 1e-5)) error stop 4
  print *, x(1), y(1), y(n)
end program
