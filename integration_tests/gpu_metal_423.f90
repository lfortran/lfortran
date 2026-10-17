! Arrays that do not start at one reach device code with their lower bounds:
! an allocatable component of a derived-type argument (also through an
! associate name), an allocatable dummy, and the component of an element of
! an array of a derived type.
module gpu_metal_423_m
  implicit none
  type :: net_t
    integer, allocatable :: nodes_(:)
    integer, allocatable :: w(:,:)
  end type
contains
  pure function node(net, k) result(r)
    type(net_t), intent(in) :: net
    integer, intent(in) :: k
    integer :: r
    r = net%nodes_(k) * 100 + lbound(net%nodes_, 1) * 10 &
      + ubound(net%nodes_, 1)
  end function

  pure function last_node(net) result(r)
    type(net_t), intent(in) :: net
    integer :: r
    associate (n => net%nodes_, output_layer => ubound(net%nodes_, 1))
      r = n(output_layer) + 10 * lbound(n, 1)
    end associate
  end function

  pure function weight(net, k) result(r)
    type(net_t), intent(in) :: net
    integer, intent(in) :: k
    integer :: r
    r = net%w(2, k) * 100 + lbound(net%w, 2) * 10 + ubound(net%w, 2) &
      + 1000 * lbound(net%w, 1)
  end function

  pure function entry(a, k) result(r)
    integer, allocatable, intent(in) :: a(:,:)
    integer, intent(in) :: k
    integer :: r
    r = a(2, k) * 100 + lbound(a, 2) * 10 + ubound(a, 2) + 1000 * lbound(a, 1)
  end function
end module

program gpu_metal_423
  use gpu_metal_423_m
  implicit none
  type(net_t) :: net, nets(2)
  integer, allocatable :: y(:,:)
  integer :: i, r1(3), r2(3), r3(3), r4(3), r5(2)

  allocate(net%nodes_(0:2), net%w(-1:2, 0:2), y(-1:2, 0:2))
  net%nodes_ = [5, 6, 7]
  net%w = 0
  net%w(2, :) = [4, 5, 6]
  y = net%w
  allocate(nets(1)%nodes_(0:1), nets(2)%nodes_(0:1))
  nets(1)%nodes_ = [1, 2]
  nets(2)%nodes_ = [3, 4]

  do concurrent (i = 1:3)
    r1(i) = node(net, i - 1)
    r2(i) = last_node(net)
    r3(i) = weight(net, i - 1)
    r4(i) = entry(y, i - 1)
  end do
  do concurrent (i = 1:2)
    r5(i) = nets(i)%nodes_(1) * 100 + lbound(nets(i)%nodes_, 1) * 10 &
      + ubound(nets(i)%nodes_, 1)
  end do

  print *, r1, r2, r3, r4, r5
  if (any(r1 /= [502, 602, 702])) error stop 1
  if (any(r2 /= 7)) error stop 2
  if (any(r3 /= [-1000 + 402, -1000 + 502, -1000 + 602])) error stop 3
  if (any(r4 /= [-1000 + 402, -1000 + 502, -1000 + 602])) error stop 4
  if (any(r5 /= [201, 401])) error stop 5
end program
