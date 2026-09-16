! `it(1)%fv` splices a type-bound function into the kernel. Its component
! reference `self%l` has to be rebound to the kernel's copy of the derived
! type, the one the kernel layout describes, together with its base.
module gpu_metal_343_m
  implicit none
  type :: t
    real, allocatable :: l(:,:)
  contains
    procedure :: fv
  end type
contains
  pure function fv(self, x) result(c)
    class(t), intent(in) :: self
    real, intent(in) :: x(:)
    real, allocatable :: c(:)
    c = [matmul(self%l, x)]
  end function
  pure function mk(n) result(r)
    integer, intent(in) :: n
    type(t), allocatable :: r(:)
    integer :: i
    allocate(r(n))
    do i = 1, n
      allocate(r(i)%l(1,3))
      r(i)%l = 1
    end do
  end function
end module
program gpu_metal_343
  use gpu_metal_343_m
  implicit none
  real :: a(3,2), b(1,2)
  integer :: j
  a = 1
  associate(it => mk(2))
    do concurrent (j = 1:2)
      b(:,j) = it(1)%fv(a(:,j))
    end do
  end associate
  if (any(b /= 3)) error stop
  print *, "ok"
end program
