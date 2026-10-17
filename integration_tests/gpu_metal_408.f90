module gpu_metal_408_m
  ! An allocatable of a device routine is given storage the host sizes
  ! before the launch when its allocations state one shape, written as its
  ! value on entry to the routine. `a` is allocated twice with the shape
  ! `m`, but `m` is changed in between through the associate name `q`, so
  ! the two allocations do not have one shape and `m` is not a value known
  ! on entry: `a` is allocated on the device heap, with the shape of each
  ! allocation, rather than given room for only the first.
  implicit none
contains
  pure real function f(n, x) result(r)
    integer, intent(in) :: n
    real, intent(in) :: x
    real, allocatable :: a(:)
    integer :: m
    m = n
    allocate(a(m))
    a = x
    r = sum(a)
    deallocate(a)
    associate (q => m)
      q = q + 4
    end associate
    allocate(a(m))
    a = x
    r = r + 10*sum(a)
  end function
end module

program gpu_metal_408
  use gpu_metal_408_m
  implicit none
  integer, parameter :: n = 64
  real :: x(n), y(n)
  integer :: i
  do i = 1, n
    x(i) = i
  end do
  do concurrent (i = 1:n)
    y(i) = f(2, x(i)) + 0
  end do
  do i = 1, n
    if (abs(y(i) - 62*x(i)) > 1e-3) error stop 1
  end do
  print *, "ok"
end program
