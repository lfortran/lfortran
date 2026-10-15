! A section that is not contiguous, handed to a procedure by a routine the
! kernel calls: to an intrinsic the device lowers to a routine of its own,
! and to a procedure of the program. The procedure is given the elements of
! the section, not those that follow its first element. The feed-forward
! loop is the shape of a neural network's inference.
module gpu_metal_425_m
  implicit none
  integer, parameter :: input_layer = 0
  type :: net_t
    real, allocatable :: w(:,:,:), b(:,:)
    integer, allocatable :: nodes_(:)
  end type
contains
  pure function weigh(a) result(r)
    real, intent(in) :: a(:,:)
    real :: r
    r = a(1, 1) + 10 * a(2, 1) + 100 * a(1, 2) + 1000 * a(2, 2)
  end function

  pure function product_of(w, x) result(y)
    real, intent(in) :: w(:,:,:), x(:)
    real :: y(2)
    y = matmul(w(1:2, 1:2, 2), x)
  end function

  pure function weighed(w) result(r)
    real, intent(in) :: w(:,:,:)
    real :: r
    r = weigh(w(1:2, 2:3, 2))
  end function

  pure function infer(self, x) result(y)
    type(net_t), intent(in) :: self
    real, intent(in) :: x(:)
    real :: y(2)
    real, allocatable :: a(:,:)
    integer :: l
    associate (w => self%w, b => self%b, n => self%nodes_, &
        output_layer => ubound(self%nodes_, 1))
      allocate(a(size(self%w, 1), input_layer:output_layer))
      a(1:n(input_layer), input_layer) = x
      do l = input_layer + 1, output_layer
        associate (z => matmul(w(1:n(l), 1:n(l-1), l), a(1:n(l-1), l-1)) &
            + b(1:n(l), l))
          a(1:n(l), l) = z
        end associate
      end do
      y = a(1:n(output_layer), output_layer)
    end associate
  end function
end module

program gpu_metal_425
  use gpu_metal_425_m
  implicit none
  type(net_t) :: net
  real :: w(3, 3, 2), xs(2, 3), ys(2, 3), yr(2, 3), s(3)
  integer :: i, j, k

  w = reshape([(real(i), i = 1, 18)], [3, 3, 2])
  xs = reshape([(real(i), i = 1, 6)], [2, 3])

  allocate(net%nodes_(0:2), net%w(3, 3, 2), net%b(3, 2))
  net%nodes_ = 2
  do k = 1, 2
    do j = 1, 3
      do i = 1, 3
        net%w(i, j, k) = i + 2*j + 3*k
      end do
    end do
  end do
  net%b = reshape([0.5, 1.5, 9.0, 2.5, 3.5, 9.0], [3, 2])

  do concurrent (i = 1:3)
    ys(:, i) = product_of(w, xs(:, i))
    s(i) = weighed(w)
  end do
  do i = 1, 3
    if (any(abs(ys(:, i) - matmul(w(1:2, 1:2, 2), xs(:, i))) > 1e-3)) &
      error stop 1
  end do
  if (any(abs(s - weigh(w(1:2, 2:3, 2))) > 1e-2)) error stop 2

  do i = 1, 3
    yr(:, i) = infer(net, xs(:, i))
  end do
  do concurrent (i = 1:3)
    ys(:, i) = infer(net, xs(:, i))
  end do
  print *, ys
  print *, yr
  if (any(abs(ys - yr) > 1e-3)) error stop 3
  if (abs(yr(1, 1) - 496.5) > 1e-3) error stop 4
end program
