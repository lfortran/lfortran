! Type-bound intrinsic operators call their bound procedures, not the
! procedures of the program that have their names.
module operator_overloading_53_m
  implicit none
  private
  public :: t
  type :: t
    integer :: x
  contains
    procedure, private :: add_t, neg_t, eq_t, and_t, add_it
    procedure, private, pass(b) :: add_ti
    generic :: operator(+) => add_t, add_ti
    generic :: operator(-) => neg_t
    generic :: operator(==) => eq_t
    generic :: operator(.and.) => and_t
  end type
contains
  function add_t(a, b) result(c)
    class(t), intent(in) :: a, b
    type(t) :: c
    c%x = a%x + 2*b%x
  end function
  function add_it(a, i) result(c)
    class(t), intent(in) :: a
    integer, intent(in) :: i
    type(t) :: c
    c%x = a%x + i
  end function
  function add_ti(i, b) result(c)
    integer, intent(in) :: i
    class(t), intent(in) :: b
    type(t) :: c
    c%x = 7*i + b%x
  end function
  function neg_t(a) result(c)
    class(t), intent(in) :: a
    type(t) :: c
    c%x = -3*a%x
  end function
  logical function eq_t(a, b)
    class(t), intent(in) :: a, b
    eq_t = a%x == b%x
  end function
  logical function and_t(a, b)
    class(t), intent(in) :: a, b
    and_t = a%x > 0 .and. b%x > 0
  end function
end module
program operator_overloading_53
  use operator_overloading_53_m
  implicit none
  type(t) :: a, b, c
  a = t(1); b = t(2)
  c = a + b
  if (c%x /= 5) error stop 1
  c = 2 + b
  if (c%x /= 16) error stop 2
  c = -a
  if (c%x /= -3) error stop 3
  if (a == b) error stop 4
  if (.not. (a .and. b)) error stop 5
  if (add_t(1) + add_ti(1) + neg_t(1) + eq_t(1) + and_t(1) /= 1500) error stop 6
  print *, "ok"
contains
  integer function add_t(i)
    integer, intent(in) :: i
    add_t = 100*i
  end function
  integer function add_ti(i)
    integer, intent(in) :: i
    add_ti = 200*i
  end function
  integer function neg_t(i)
    integer, intent(in) :: i
    neg_t = 300*i
  end function
  integer function eq_t(i)
    integer, intent(in) :: i
    eq_t = 400*i
  end function
  integer function and_t(i)
    integer, intent(in) :: i
    and_t = 500*i
  end function
end program
