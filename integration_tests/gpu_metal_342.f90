! An element of an associate name for an array of derived type, handed to a
! function that multiplies by the element's allocatable rank-2 component,
! offloads without a CPU fallback: `it(k)%l` is the component of `s(k)`.
program gpu_metal_342
  implicit none
  type :: t
    real, allocatable :: l(:,:)
  end type
  type(t) :: s(2)
  real :: a(3,2), b(1,2), c(1,2)
  integer :: j
  a = 1
  allocate(s(1)%l(1,3), s(2)%l(1,3))
  s(1)%l = 1
  s(2)%l = 2
  associate(it => s)
    do concurrent (j = 1:2)
      b(:,j) = f(it(1), a(:,j))
      c(:,j) = f(it(2), a(:,j))
    end do
  end associate
  print *, b
  print *, c
  if (any(b /= 3)) error stop
  if (any(c /= 6)) error stop
contains
  pure function f(self, x) result(r)
    type(t), intent(in) :: self
    real, intent(in) :: x(:)
    real, allocatable :: r(:)
    r = [matmul(self%l, x)]
  end function
end program
