module use_04_mod
  implicit none
  private
contains
  subroutine increment(x)
    integer, intent(inout) :: x
    x = x + 2
  end subroutine
end module use_04_mod

program use_04
  use use_04_mod
  implicit none
  integer :: x
  x = 5
  call increment(x)
  if (x /= 6) error stop
contains
  subroutine increment(x)
    integer, intent(out) :: x
    x = x + 1
  end subroutine
end program