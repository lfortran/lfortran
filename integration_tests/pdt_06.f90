module pdt_06_outer_m
  use pdt_06_inner_m, only : inner
  implicit none

  type outer(k)
    integer, kind :: k = kind(1.)
    type(inner(k)) :: a
  end type

  interface outer
    module function make_outer(a) result(r)
      type(inner), intent(in) :: a
      type(outer) r
    end function
  end interface
end module

program pdt_06
  use pdt_06_inner_m, only : inner
  use pdt_06_outer_m, only : outer
  implicit none
  type(inner) :: x
  type(outer) :: y
  x%v = 2.5
  y%a = x
  if (abs(y%a%v - 2.5) > 1e-5) error stop
  print *, "ok"
end program
