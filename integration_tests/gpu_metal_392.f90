module gpu_metal_392_m
  ! A type-bound function called from an offloaded loop with an array of
  ! the thread's own memory. The call is retargeted at a copy of the
  ! function for that memory space, which is not a procedure of the type,
  ! so the call becomes an ordinary call to the copy, also when it is made
  ! from another device routine.
  implicit none
  type :: scale_t
    real :: f
  contains
    procedure :: scaled
  end type
contains
  pure function scaled(self, w) result(r)
    class(scale_t), intent(in) :: self
    real, intent(in) :: w(:)
    real :: r
    integer :: i
    r = 0
    do i = 1, size(w)
      r = r + self%f*w(i)
    end do
  end function

  pure function applied(s, w) result(r)
    type(scale_t), intent(in) :: s
    real, intent(in) :: w(:)
    real :: r
    r = s%scaled(w) + 1
  end function
end module

program gpu_metal_392
  use gpu_metal_392_m
  implicit none
  type(scale_t) :: s
  real :: y(8)
  integer :: i
  s%f = 2
  do concurrent (i = 1:8)
    block
      real :: w(3)
      w = real(i)
      y(i) = s%scaled(w)
    end block
  end do
  do i = 1, 8
    if (abs(y(i) - 6*i) > 1e-4) error stop 1
  end do
  do concurrent (i = 1:8)
    block
      real :: w(3)
      w = real(i)
      y(i) = applied(s, w)
    end block
  end do
  do i = 1, 8
    if (abs(y(i) - (6*i + 1)) > 1e-4) error stop 2
  end do
  print *, "ok"
end program
