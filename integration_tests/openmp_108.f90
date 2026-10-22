module openmp_108_m
  ! A region outlined from an ASSOCIATE or BLOCK construct receives the
  ! descriptor of an assumed-shape dummy argument of the procedure, which
  ! the procedure keeps, also for a non-contiguous actual argument.
  implicit none
contains
  subroutine s(x, n)
    real, intent(inout) :: x(:)
    integer, intent(in) :: n
    integer :: i
    associate (b => n)
      do concurrent (i = 1:b)
        x(i) = x(i) + i
      end do
    end associate
  end subroutine

  subroutine t(x)
    real, intent(inout) :: x(:)
    integer :: i
    block
      integer :: m
      m = size(x)
      !$omp parallel do
      do i = 1, m
        x(i) = 2*x(i)
      end do
      !$omp end parallel do
    end block
  end subroutine
end module

program openmp_108
  use openmp_108_m, only: s, t
  implicit none
  real :: x(10), y(10)
  integer :: k
  x = 0
  do k = 1, 3
    call s(x, 3)
  end do
  if (abs(sum(x) - 18) > 1e-6) error stop "contiguous"
  y = 1
  call s(y(1::2), 4)
  if (abs(sum(y) - 20) > 1e-6) error stop "strided"
  if (abs(y(3) - 3) > 1e-6 .or. abs(y(4) - 1) > 1e-6) error stop "strided elements"
  call t(y(2::2))
  if (abs(sum(y) - 25) > 1e-6) error stop "block"
  print *, sum(x), sum(y)
end program
