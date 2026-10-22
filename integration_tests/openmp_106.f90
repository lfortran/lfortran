module openmp_106_mod
  implicit none
contains
  integer function g(k)
    integer, intent(in) :: k
    g = k
  end function
end module

program openmp_106
  ! A pointer bounds-spec with lower bounds that are function references,
  ! in a parallel loop with a private pointer. Each thread evaluates the
  ! data target and the lower bounds of its own iterations.
  use openmp_106_mod
  implicit none
  integer, parameter :: n = 1000000
  real, target :: a(10), b(3, 4)
  real, pointer :: p(:), q(:, :)
  integer, allocatable :: lb(:, :), ub(:, :)
  integer :: i, nbad
  allocate(lb(3, n), ub(3, n))
  a = 1
  b = 2
  !$omp parallel do private(p, q) num_threads(4)
  do i = 1, n
    p(g(i):) => a
    q(g(-i):, g(i + 1):) => b(:, g(mod(i, 2) + 1):g(mod(i, 2) + 3))
    lb(1, i) = lbound(p, 1)
    ub(1, i) = ubound(p, 1)
    lb(2, i) = lbound(q, 1)
    ub(2, i) = ubound(q, 1)
    lb(3, i) = lbound(q, 2)
    ub(3, i) = ubound(q, 2)
  end do
  !$omp end parallel do
  nbad = 0
  do i = 1, n
    if (lb(1, i) /= i .or. ub(1, i) /= i + 9 .or. &
        lb(2, i) /= -i .or. ub(2, i) /= -i + 2 .or. &
        lb(3, i) /= i + 1 .or. ub(3, i) /= i + 3) then
      if (nbad == 0) print *, "iteration", i, lb(:, i), ub(:, i)
      nbad = nbad + 1
    end if
  end do
  print *, "wrong bounds:", nbad
  if (nbad /= 0) error stop
end program
