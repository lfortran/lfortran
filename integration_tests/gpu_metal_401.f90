module gpu_metal_401_m
  ! An allocatable of device code whose extent is an array intrinsic of an
  ! argument, `allocate(a(maxval(n)))`, also through associate names bound
  ! to a component and to its bounds. A device has no heap, so the array is
  ! a per-thread workspace the host sizes before the launch: the host calls
  ! the routine shared lowering gives the intrinsic, as the device does.
  implicit none
  type :: net_t
    integer, allocatable :: n(:)
  end type
contains
  pure real function widest(net, x)
    type(net_t), intent(in) :: net
    real, intent(in) :: x
    real, allocatable :: a(:), b(:)
    allocate(a(maxval(net%n)), b(sum(net%n)))
    a = x
    b = 1
    widest = sum(a) + sum(b)
  end function

  pure real function layers(net, x)
    type(net_t), intent(in) :: net
    real, intent(in) :: x
    real, allocatable :: a(:,:)
    integer, parameter :: first = 0
    integer :: j, k
    associate (n => net%n, last => size(net%n) - 1)
      allocate(a(maxval(n), first:last))
      a = x
      layers = 0
      do j = first, last
        do k = 1, size(a, 1)
          layers = layers + a(k, j)
        end do
      end do
    end associate
  end function
end module

program gpu_metal_401
  use gpu_metal_401_m
  implicit none
  type(net_t) :: net
  real :: o(3), p(3)
  integer :: i
  allocate(net%n(3))
  net%n = [2, 5, 3]
  do concurrent (i = 1:3)
    o(i) = 1 + widest(net, real(i))
    p(i) = 1 + layers(net, real(i))
  end do
  do i = 1, 3
    if (abs(o(i) - (1 + 5 * i + 10)) > 1e-5) error stop 1
    if (abs(p(i) - (1 + 15 * i)) > 1e-5) error stop 2
  end do
  print *, o, p
end program
