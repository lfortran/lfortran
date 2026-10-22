module openmp_107_m
  ! A do concurrent or parallel do inside ASSOCIATE or BLOCK is outlined in
  ! the scope of the construct.
  implicit none
contains
  real function f(n) result(r)
    integer, intent(in) :: n
    integer :: i
    real :: y(5)
    y = 0
    block
      associate (b => n)
        do concurrent (i = 1:b)
          y(i) = 2*i
        end do
      end associate
    end block
    r = sum(y)
  end function
end module

program openmp_107
  use openmp_107_m, only: f
  implicit none
  integer :: i, n
  real :: x(10)
  n = 3
  x = 0
  associate (b => n)
    do concurrent (i = 1:b)
      x(i) = i
    end do
  end associate
  if (abs(sum(x) - 6) > 1e-6) error stop "do concurrent"
  x = 0
  associate (b => n)
    !$omp parallel do
    do i = 1, b
      x(i) = i
    end do
    !$omp end parallel do
  end associate
  if (abs(sum(x) - 6) > 1e-6) error stop "parallel do"
  x = 0
  associate (b => n + 1)
    !$omp parallel do
    do i = 1, b
      x(i) = i
    end do
    !$omp end parallel do
  end associate
  if (abs(sum(x) - 10) > 1e-6) error stop "expression"
  x = 0
  block
    integer :: c
    c = n
    !$omp parallel do
    do i = 1, c
      x(i) = i
    end do
    !$omp end parallel do
  end block
  if (abs(sum(x) - 6) > 1e-6) error stop "block"
  if (abs(f(n) - 12) > 1e-6) error stop "function"
  print *, sum(x)
end program
