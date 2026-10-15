module gpu_metal_383_m
  ! Test: `error stop` in procedures an offloaded loop reaches, on branches
  ! the data never takes. The stops are lowered into writes to a status
  ! buffer the host reads after the launch, so the loops offload on every
  ! device, Metal included, and compute what they compute on the CPU.
  implicit none
  type :: activation_t
    integer :: selection = 1
  contains
    procedure :: evaluate
  end type
contains
  elemental real function evaluate(self, x) result(y)
    class(activation_t), intent(in) :: self
    real, intent(in) :: x
    select case (self%selection)
    case (1)
      y = 2*x
    case (2)
      y = negated(x)
    case default
      error stop "evaluate: unknown activation"
    end select
  end function

  pure real function negated(x) result(y)
    real, intent(in) :: x
    if (x > 1000) error stop 42
    y = -x
  end function
end module

program gpu_metal_383
  use gpu_metal_383_m
  implicit none
  integer, parameter :: n = 8
  real :: z(3, n), w(3, n)
  integer :: i, k(n)
  type(activation_t) :: act

  do i = 1, n
    z(:, i) = real(i)
  end do

  ! An elemental type-bound function applied to a section.
  act%selection = 1
  do concurrent (i = 1:n)
    w(:, i) = act%evaluate(z(:, i))
  end do
  do i = 1, n
    if (any(abs(w(:, i) - 2*real(i)) > 1e-6)) error stop "evaluate 1"
  end do

  ! Through a second procedure that can stop, in a BLOCK.
  act%selection = 2
  do concurrent (i = 1:n)
    block
      real :: t(3)
      t = act%evaluate(z(:, i))
      w(:, i) = t
    end block
  end do
  do i = 1, n
    if (any(abs(w(:, i) + real(i)) > 1e-6)) error stop "evaluate 2"
  end do

  ! In the loop body itself, with a stop code computed at run time.
  do concurrent (i = 1:n)
    k(i) = 3*i
    if (k(i) > 1000) error stop k(i)
  end do
  if (sum(k) /= 108) error stop "body"

  print *, "PASS"
end program
