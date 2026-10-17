module gpu_metal_410_m
  ! An array whose size the iteration works out itself, from the values it
  ! reads, cannot be sized by the host before the launch: it is allocated
  ! on the device heap, in the kernel and in a procedure the kernel calls.
  implicit none
contains
  pure real function total(v)
    real, intent(in) :: v(:)
    integer :: k
    total = 0
    do k = 1, size(v)
      total = total + k * v(k)
    end do
  end function

  pure real function big(x) result(r)
    real, intent(in) :: x(:)
    real, allocatable :: w(:)
    integer :: c, j
    c = 0
    do j = 1, size(x)
      if (x(j) > 0.5) c = c + 1
    end do
    allocate(w(c))
    c = 0
    do j = 1, size(x)
      if (x(j) > 0.5) then
        c = c + 1
        w(c) = x(j)
      end if
    end do
    r = total(w) + size(w)
    ! A second allocation, of another size and lower bound.
    deallocate(w)
    if (allocated(w)) error stop 10
    allocate(w(-1:c))
    w = 1
    r = r + sum(w) + lbound(w, 1) + ubound(w, 1)
  end function
end module

program gpu_metal_410
  use gpu_metal_410_m
  implicit none
  integer, parameter :: n = 6
  real :: x(10, n), r(n), s(n), q(n), e
  integer :: i, k, c
  do i = 1, n
    do k = 1, 10
      x(k, i) = mod(k * 7 + i * 3, 11) / 10.0
    end do
  end do
  do concurrent (i = 1:n)
    block
      real, allocatable :: w(:), m(:, :)
      integer :: c, j
      c = 0
      do j = 1, 10
        if (x(j, i) > 0.5) c = c + 1
      end do
      allocate(w(c), m(c, 2))
      c = 0
      do j = 1, 10
        if (x(j, i) > 0.5) then
          c = c + 1
          w(c) = x(j, i)
          m(c, 1) = j
          m(c, 2) = -x(j, i)
        end if
      end do
      r(i) = sum(w) + size(w) + sum(m(:, 1)) + size(m, 1)
      ! A procedure gets the array whole.
      s(i) = total(w) + m(size(m, 1), 2)
    end block
  end do
  do concurrent (i = 1:n)
    q(i) = 1 + big(x(:, i))
  end do
  do i = 1, n
    c = count(x(:, i) > 0.5)
    e = sum(x(:, i), x(:, i) > 0.5) + c
    do k = 1, 10
      if (x(k, i) > 0.5) e = e + k
    end do
    e = e + c
    if (abs(r(i) - e) > 1e-4) error stop 1
    e = 0
    c = 0
    do k = 1, 10
      if (x(k, i) > 0.5) then
        c = c + 1
        e = e + c * x(k, i)
      end if
    end do
    if (abs(s(i) - (e - x(findloc(x(:, i) > 0.5, .true., 1, back=.true.), i))) > 1e-4) error stop 2
    if (abs(q(i) - (1 + e + c + (c + 2) - 1 + c)) > 1e-4) error stop 3
  end do
  print *, r
  print *, s
  print *, q
end program
