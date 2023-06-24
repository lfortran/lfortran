module template_add_04_m
  implicit none
  private
  public :: add_t

  requirement R(T, F)
      type, deferred :: T
      function F(x, y) result(z)
          type(T), intent(in) :: x, y
          type(T) :: z
      end function
  end requirement

  template add_t(T, F, mult)
      requires R(T, F)
      integer :: mult
      private
      public :: add_generic
  contains
      function add_generic(x, y) result(z)
          type(T), intent(in) :: x, y
          type(T) :: z
          integer :: i
          z = F(x, y)
          do i = 1,mult-1
              z = F(z, F(x,y))
          end do
      end function
  end template

contains

  real function func_arg_real(x, y) result(z)
      real, intent(in) :: x, y
      z = x + y
  end function

  integer function func_arg_int(x, y) result(z)
      integer, intent(in) :: x, y
      z = x + y
  end function

  subroutine test_template()
      integer, parameter :: n = 10
      instantiate add_t(real, func_arg_real, n), only: add_real => add_generic
      real :: x, y
      integer :: a, b
      x = 5.1
      y = 7.2
      print*, "The result is ", add_real(x, y)
  end subroutine
end module

program template_add_01
use template_add_01_m
implicit none

call test_template()

end program