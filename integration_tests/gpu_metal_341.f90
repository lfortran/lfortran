program gpu_metal_341
! `associate(it => s)` over an array of derived type binds `it` to a cast of
! `s`; the kernel has to address the allocatable component `it(1)%l` through
! `s` itself to find the buffers its layout describes.
implicit none
type :: t
    real, allocatable :: l(:,:)
end type
type(t) :: s(1)
real :: a(3,2), b(1,2)
integer :: j
a = 1
allocate(s(1)%l(1,3))
s(1)%l = 1
associate(it => s)
    do concurrent (j = 1:2)
        b(:,j) = f(it(1), a(:,j))
    end do
end associate
print *, b
if (any(b /= 3)) error stop
contains
    pure function f(self, x) result(c)
    type(t), intent(in) :: self
    real, intent(in) :: x(:)
    real, allocatable :: c(:)
    c = [matmul(self%l, x)]
    end function
end program
