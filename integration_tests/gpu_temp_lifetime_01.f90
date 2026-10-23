module gpu_temp_lifetime_01_m
  ! A polymorphic dummy is handed to the device as a copy of its declared
  ! type's components. Here the only component is an allocatable array,
  ! which the device gets on its own, so the copy holds nothing to copy
  ! back. The copy lives in the `associate` block of the loop, and is gone
  ! when the host next waits for the device: nothing may be copied into it
  ! then, onto what the host keeps in that storage afterwards.
  implicit none
  type :: t
    real, allocatable :: w(:)
  end type
contains
  pure integer function n(self)
    class(t), intent(in) :: self
    n = size(self%w)
  end function

  subroutine s(self, ok)
    class(t), intent(inout) :: self
    logical, intent(out) :: ok
    integer :: i
    real :: total
    associate(w => self%w)
      do concurrent (i = 1:size(w))
        w(i) = w(i) + n(self) + i
      end do
    end associate
    block
      integer(8) :: a1, a2, a3, a4, a5, a6, a7, a8
      integer(8) :: a9, a10, a11, a12, a13, a14, a15, a16
      a1 = 42
      a2 = 42
      a3 = 42
      a4 = 42
      a5 = 42
      a6 = 42
      a7 = 42
      a8 = 42
      a9 = 42
      a10 = 42
      a11 = 42
      a12 = 42
      a13 = 42
      a14 = 42
      a15 = 42
      a16 = 42
      total = sum(self%w)
      ok = total == 18
      ok = ok .and. a1 == 42 .and. a2 == 42 .and. a3 == 42 .and. a4 == 42
      ok = ok .and. a5 == 42 .and. a6 == 42 .and. a7 == 42 .and. a8 == 42
      ok = ok .and. a9 == 42 .and. a10 == 42 .and. a11 == 42 .and. a12 == 42
      ok = ok .and. a13 == 42 .and. a14 == 42 .and. a15 == 42 .and. a16 == 42
    end block
  end subroutine
end module

program gpu_temp_lifetime_01
  use gpu_temp_lifetime_01_m
  implicit none
  type(t) :: x
  logical :: ok
  allocate(x%w(3))
  x%w = 1
  call s(x, ok)
  if (any(x%w /= [5., 6., 7.])) error stop 1
  if (.not. ok) error stop 2
  print *, "ok"
end program
