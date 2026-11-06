module allocatable_layout_m
  implicit none
  type storage_t
    real, allocatable :: a(:,:)
    integer, allocatable :: counts(:,:)
    complex, allocatable :: c(:,:)
    real, pointer :: p(:,:)
  contains
    procedure :: update
  end type
contains
  subroutine update(self, x)
    class(storage_t), intent(inout), target :: self
    real, intent(in) :: x(:)
    integer :: j
    do j = lbound(self%a,2), ubound(self%a,2)
      self%a(:,j) = 2*self%a(:,j) + x(:)
    end do
    self%counts = self%counts + 2
    self%c = self%c * cmplx(0,1)
    self%p = 3*self%p
  end subroutine
end module

program array_allocatable_layout
  use allocatable_layout_m
  implicit none
  type(storage_t), target :: s
  real, target :: data(10,4)
  real :: x(10)
  real :: expected
  integer :: i, j
  allocate(s%a(-2:2,4:6), s%counts(-1:1,2:3), s%c(-1:1,2:3))
  s%a = 1
  s%counts = 1
  s%c = cmplx(1,2)
  data = 1
  s%p => data(10:2:-2,2:4)
  x = [(real(i),i=1,size(x))]
  call s%update(x(10:2:-2))
  do j = 4, 6
    do i = -2, 2
      if (s%a(i,j) /= 2 + x(10-2*(i+2))) error stop 1
    end do
  end do
  if (any(s%counts /= 3)) error stop 2
  if (any(s%c /= cmplx(-2,1))) error stop 7
  do j = 1, 4
    do i = 1, 10
      if (mod(i,2) == 0 .and. j >= 2) then
        if (data(i,j) /= 3) error stop 3
      else
        if (data(i,j) /= 1) error stop 4
      end if
    end do
  end do
  s%p => s%a(2:-2:-2,6:4:-1)
  s%p = s%p + 100
  do j = 4, 6
    do i = -2, 2
      expected = 2 + x(10-2*(i+2))
      if (mod(i,2) == 0) expected = expected + 100
      if (s%a(i,j) /= expected) error stop 8
    end do
  end do
  s%p => data(10:2:-2,2:4)
  deallocate(s%a)
  allocate(s%a(-2:-3,4:6))
  call s%update(x(1:0))
  if (size(s%a) /= 0 .or. any(s%counts /= 5)) error stop 5
  if (any(s%p /= 9)) error stop 6
  if (any(s%c /= cmplx(-1,-2))) error stop 9
end program
