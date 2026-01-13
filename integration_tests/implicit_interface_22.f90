module implicit_interface_22_mod
  implicit none
  interface generic_sub
    module procedure real_sub
  end interface
contains
  subroutine real_sub(f, n)
    integer, intent(in) :: n
    interface
      subroutine f(x)
        real, intent(in) :: x
      end subroutine
    end interface
    real :: x
    x = 1.0
    call f(x)
    if (n /= 3) error stop
    print *, "real_sub called with n =", n
  end subroutine
end module

program test
  use implicit_interface_22_mod
  implicit none
  external :: my_f
  call generic_sub(my_f, 3)
  print *, "PASS"
end program

subroutine my_f(x)
  real, intent(in) :: x
  print *, "my_f called with x =", x
end subroutine
