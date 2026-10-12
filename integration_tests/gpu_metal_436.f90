module gpu_metal_436_m
  ! A run-time sized array of device code is sized by the host before the
  ! launch, from the values the dummy arguments of its routine have on
  ! entry. An array whose extent is evaluated later, where a BLOCK starts
  ! or a call is made, after the routine changed a dummy argument the
  ! extent reads, cannot be sized that way: the GPU backends report an
  ! error rather than give every thread a slice that is too small.
  implicit none
contains
  pure subroutine in_block(n, x, r)
    integer, value :: n
    real, intent(in) :: x
    real, intent(out) :: r
    integer :: j
    n = n + 4
    block
      real :: w(n)
      do j = 1, n
        w(j) = x + j
      end do
      r = sum(w)
    end block
  end subroutine

  pure subroutine inner(k, x, r)
    integer, intent(in) :: k
    real, intent(in) :: x
    real, intent(out) :: r
    real :: w(k)
    integer :: j
    do j = 1, k
      w(j) = x + j
    end do
    r = sum(w)
  end subroutine

  pure subroutine before_call(k, x, r)
    integer, value :: k
    real, intent(in) :: x
    real, intent(out) :: r
    k = k + 4
    call inner(k, x, r)
  end subroutine
end module

program gpu_metal_436
  use gpu_metal_436_m
  implicit none
  integer, parameter :: m = 64
  real :: x(m), r(m), s(m)
  integer :: i, n0
  do i = 1, m
    x(i) = i
  end do
  n0 = 2
  do concurrent (i = 1:m)
    call in_block(n0, x(i), r(i))
  end do
  do concurrent (i = 1:m)
    call before_call(n0, x(i), s(i))
  end do
  do i = 1, m
    if (abs(r(i) - (6*x(i) + 21)) > 1e-3) error stop 1
    if (abs(s(i) - (6*x(i) + 21)) > 1e-3) error stop 2
  end do
  print *, "ok"
end program
