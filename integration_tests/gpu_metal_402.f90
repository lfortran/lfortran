module gpu_metal_402_m
  ! The temporaries of an array expression over sections whose extent the
  ! index of a loop of the device routine picks, as in the feed-forward
  ! loop of a neural network: `v(1:n(l)) * a(1:n(l), l-1)`. A device has
  ! no heap, and the extent is known only inside the loop, so each
  ! temporary points to the leading part of a buffer sized on the host by
  ! the extent of the array the section is taken of, which no iteration
  ! exceeds.
  implicit none
  type :: net_t
    real, allocatable :: v(:), b(:)
    integer, allocatable :: n(:)
  end type
contains
  pure real function infer(net, x)
    type(net_t), intent(in) :: net
    real, intent(in) :: x(:)
    real, allocatable :: a(:,:)
    integer :: l
    associate(v => net%v, b => net%b, n => net%n, last => size(net%n))
      allocate(a(maxval(n), last))
      a = 0
      a(1:n(1), 1) = x
      do l = 2, last
        associate(z => v(1:n(l)) * a(1:n(l), l-1) + b(1:n(l)))
          a(1:n(l), l) = z
        end associate
      end do
      infer = sum(a(1:n(last), last))
    end associate
  end function

  ! The same, with loops over the elements.
  real function reference(net, x)
    type(net_t), intent(in) :: net
    real, intent(in) :: x(:)
    real :: a(3, 3)
    integer :: i, l
    a = 0
    a(1:net%n(1), 1) = x
    do l = 2, size(net%n)
      do i = 1, net%n(l)
        a(i, l) = net%v(i) * a(i, l - 1) + net%b(i)
      end do
    end do
    reference = 0
    do i = 1, net%n(size(net%n))
      reference = reference + a(i, size(net%n))
    end do
  end function
end module

program gpu_metal_402
  use gpu_metal_402_m
  implicit none
  type(net_t) :: net
  real :: x(2, 4), o(4), e(4)
  integer :: i
  allocate(net%n(3), net%v(3), net%b(3))
  net%n = [2, 3, 2]
  do i = 1, 3
    net%b(i) = 0.5 * i - 1
    net%v(i) = 0.1 * i + 0.3
  end do
  do i = 1, 4
    x(:, i) = [real(i), 1. - i]
  end do
  do concurrent (i = 1:4)
    o(i) = 1 + infer(net, x(:, i))
  end do
  do i = 1, 4
    e(i) = 1 + reference(net, x(:, i))
  end do
  print *, o
  print *, e
  if (any(abs(o - e) > 1e-4 * (1 + abs(e)))) error stop
end program
