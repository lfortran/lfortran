! A type-bound operator calls its bound procedure, not a procedure of the
! program that has its name.
module operator_overloading_52_m
  implicit none
  private
  public :: t
  type :: t
    integer :: x
  contains
    procedure, private :: plus_t, neg_t, cat_t
    generic :: operator(.plus.) => plus_t
    generic :: operator(.neg.) => neg_t
    generic :: operator(//) => cat_t
  end type
contains
  function plus_t(a, b) result(c)
    class(t), intent(in) :: a, b
    type(t) :: c
    c%x = a%x + 2*b%x
  end function
  function neg_t(a) result(c)
    class(t), intent(in) :: a
    type(t) :: c
    c%x = -3*a%x
  end function
  function cat_t(a, b) result(c)
    class(t), intent(in) :: a, b
    type(t) :: c
    c%x = 10*a%x + b%x
  end function
end module
program operator_overloading_52
  use operator_overloading_52_m
  implicit none
  type(t) :: a, b, c
  a = t(1); b = t(2)
  c = a .plus. b
  if (c%x /= 5) error stop 1
  c = .neg. a
  if (c%x /= -3) error stop 2
  c = a // b
  if (c%x /= 12) error stop 3
  if (plus_t(1) /= 100) error stop 4
  if (neg_t(1) /= 200) error stop 5
  if (cat_t(1) /= 300) error stop 6
  print *, "ok"
contains
  integer function plus_t(i)
    integer, intent(in) :: i
    plus_t = 100*i
  end function
  integer function neg_t(i)
    integer, intent(in) :: i
    neg_t = 200*i
  end function
  integer function cat_t(i)
    integer, intent(in) :: i
    cat_t = 300*i
  end function
end program
