module m
  implicit none
contains
  subroutine s(x)
    integer, private :: x
    print *, x
  end subroutine
end module
