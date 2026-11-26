module continue_compilation_forall_impure_m
  implicit none
  integer :: cnt = 0
contains
  real function f(i)
    integer, intent(in) :: i
    cnt = cnt + 1
    f = real(i)
  end function
  integer function g(i)
    integer, intent(in) :: i
    cnt = cnt + 1
    g = i
  end function
  logical function ok(i)
    integer, intent(in) :: i
    cnt = cnt + 1
    ok = i > 1
  end function
end module
program continue_compilation_forall_impure
  use continue_compilation_forall_impure_m
  implicit none
  integer :: i
  real :: x(3), y(3)
  complex :: za(3)
  za = (1.0, 1.0)
  forall (i = 1:3) za(i)%im = f(i)
  forall (i = 1:3) x(i) = f(g(i))
  forall (i = 1:3, ok(i)) x(i) = 1.0
  forall (i = 1:3)
    x(i) = real(i)
    y(i) = f(i)
  end forall
end program
