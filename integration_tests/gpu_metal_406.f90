module gpu_metal_406_m
  implicit none
  integer, parameter :: input_layer = 0
  type :: net_t
    real, allocatable :: w(:,:,:), b(:,:)
    integer, allocatable :: nodes_(:)
  end type
contains
  pure function g(a) result(r)
    integer, intent(in) :: a(:)
    integer :: r
    r = maxval(a)
  end function

  pure function infer(self, x) result(y)
    type(net_t), intent(in) :: self
    real, intent(in) :: x(:)
    real :: y(2)
    real, allocatable :: a(:,:)
    integer :: l
    associate(w => self%w, b => self%b, n => self%nodes_, &
        output_layer => ubound(self%nodes_, 1))
      allocate(a(maxval(n), input_layer:output_layer))
      a(1:n(input_layer), input_layer) = x
      do l = input_layer + 1, output_layer
        associate(z => matmul(w(1:n(l), 1:n(l-1), l), a(1:n(l-1), l-1)) &
            + b(1:n(l), l))
          a(1:n(l), l) = z
        end associate
      end do
      y = a(1:n(output_layer), output_layer)
    end associate
  end function
end module

program gpu_metal_406
  use gpu_metal_406_m
  implicit none
  type(net_t) :: net
  integer :: y(2, 3), res(3), i, j, k
  real :: xs(2, 3), ys(2, 3), yr(2, 3)
  y = reshape([1, 5, 7, 2, 3, 9], [2, 3])
  do concurrent (i = 1:3)
    res(i) = g(y(:, i))
  end do
  print *, res, g([4, 8])
  if (any(res /= [5, 7, 9])) error stop 1
  if (g([4, 8]) /= 8) error stop 2

  allocate(net%nodes_(0:2), net%w(2, 2, 2), net%b(2, 2))
  net%nodes_ = 2
  do k = 1, 2
    do j = 1, 2
      do i = 1, 2
        net%w(i, j, k) = i + 2*j + 3*k
      end do
    end do
  end do
  net%b = reshape([0.5, 1.5, 2.5, 3.5], [2, 2])
  do i = 1, 3
    xs(:, i) = [real(i), real(-i) * 0.5]
    yr(:, i) = infer(net, xs(:, i))
  end do
  do concurrent (i = 1:3)
    ys(:, i) = infer(net, xs(:, i))
  end do
  print *, ys
  if (any(abs(ys - yr) > 1e-3)) error stop 3
  if (abs(yr(1, 1) - 69) > 1e-3) error stop 4
end program
