program intrinsics_402
  implicit none

  intrinsic :: iabs
  call xub (iabs, 42)
  call xub (iabs, -99)

contains

subroutine xub (ifn, val)
  implicit none

  interface
    function ifn(x) result(r)
      integer, intent(in) :: x
      integer :: r
    end function
  end interface
  integer :: val
  integer :: result

  result = ifn (val)
  print *, result

  if (val == 42 .and. result /= 42) error stop "Expected 42"
  if (val == -99 .and. result /= 99) error stop "Expected 99"

end subroutine

end program
