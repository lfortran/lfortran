! A section of a rank-2 array through an associate name, in device code: the
! leading dimension of the array the name stands for strides the section,
! whether the name is bound to a whole component or to a section that is
! not contiguous.
module gpu_metal_399_m
  implicit none
  type :: net_t
    real, allocatable :: b(:,:)
    integer, allocatable :: n(:)
  end type
contains
  pure function layer_sum(net, l) result(r)
    type(net_t), intent(in) :: net
    integer, intent(in) :: l
    real :: r
    associate (b => net%b, n => net%n)
      r = sum(b(1:n(l), l))
    end associate
  end function

  pure function corner(w, k) result(r)
    real, intent(in) :: w(:,:,:)
    integer, intent(in) :: k
    real :: r
    associate (z => w(1:2, 1:2, k))
      r = z(1, 1) + 10 * z(2, 1) + 100 * z(1, 2) + 1000 * z(2, 2)
    end associate
  end function
end module

program gpu_metal_399
  use gpu_metal_399_m
  implicit none
  type(net_t) :: net
  real :: w(3, 3, 2), r1(3), r2(2)
  integer :: i

  allocate(net%b(3, 3), net%n(3))
  net%n = [1, 2, 3]
  net%b = reshape([(real(i), i = 1, 9)], [3, 3])
  w = reshape([(real(i), i = 1, 18)], [3, 3, 2])

  do concurrent (i = 1:3)
    r1(i) = layer_sum(net, i)
  end do
  do concurrent (i = 1:2)
    r2(i) = corner(w, i)
  end do

  print *, r1, r2
  if (any(abs(r1 - [1.0, 9.0, 24.0]) > 1e-4)) error stop 1
  if (abs(r2(1) - 5421.0) > 1e-2) error stop 2
  if (abs(r2(2) - 15420.0) > 1e-2) error stop 3
end program
