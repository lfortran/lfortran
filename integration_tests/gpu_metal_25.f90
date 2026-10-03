program gpu_metal_25
! Test: do concurrent with array sized by derived-type member (x%n)
implicit none
type :: t
  integer :: n
end type
type(t) :: a
a%n = 3
call sub(a)
contains
  subroutine sub(x)
    type(t), intent(in) :: x
    integer :: i, s(x%n)
    do concurrent(i = 1:x%n)
      s(i) = i * 2
    end do
    if (s(1) /= 2) error stop
    if (s(2) /= 4) error stop
    if (s(3) /= 6) error stop
  end subroutine
end program
