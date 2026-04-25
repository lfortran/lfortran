module pdt_08_m
  implicit none
  integer, parameter :: wp = kind(1.)

  type t(k)
    integer, kind :: k = wp
    real(k) :: val
  end type

  interface
    module subroutine sub(x)
      class(t), intent(in) :: x
    end subroutine
  end interface
end module

program pdt_08
  use pdt_08_m
  implicit none
  type(t) :: x
  x%val = 3.14
  if (abs(x%val - 3.14) > 1e-5) error stop
  print *, "ok"
end program
