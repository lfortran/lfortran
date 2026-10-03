module pdt_07_m
  implicit none
  integer, parameter :: wp = 4
end module

module pdt_07_n
  use pdt_07_m, only : wp
  implicit none

  type t(k)
    integer, kind :: k = wp
    integer(kind=k) :: val
  end type

  interface
    module subroutine foo(x)
      type(t), intent(out) :: x
    end subroutine
  end interface
end module

program pdt_07
  use pdt_07_n
  implicit none
  type(t) :: x
  x%val = 42
  if (x%k /= 4) error stop
  if (x%val /= 42) error stop
  print *, "ok"
end program
