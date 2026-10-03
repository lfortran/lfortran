module arrays_105_mod
  implicit none
  type :: t
    integer :: x = 0
  end type

  interface t
    module procedure construct
  end interface

contains

  pure function construct(x) result(res)
    integer, intent(in) :: x
    type(t) :: res
    res%x = x
  end function

  pure function f(a) result(r)
    type(t), intent(in) :: a
    integer :: r
    r = a%x
  end function
end module

program arrays_105
  use arrays_105_mod
  implicit none
  call sub
contains
  subroutine sub
    real :: arr(f(t(3)))
    integer :: i
    do i = 1, size(arr)
      arr(i) = real(i)
    end do
    if (size(arr) /= 3) error stop
    if (abs(arr(1) - 1.0) > 1e-6) error stop
    if (abs(arr(2) - 2.0) > 1e-6) error stop
    if (abs(arr(3) - 3.0) > 1e-6) error stop
  end subroutine
end program
