program intrinsics_427
  implicit none
  type :: t
    integer :: n(3)
  end type
  type(t) :: x
  x%n = [2, 5, 3]
  call s(x)
contains
  subroutine s(self)
    type(t), intent(in) :: self
    real :: a(maxval(self%n))
    integer :: i
    if (size(a) /= 5) error stop
    do i = 1, size(a)
      a(i) = real(i)
    end do
    if (abs(a(5) - 5.0) > 1e-6) error stop
  end subroutine
end program
