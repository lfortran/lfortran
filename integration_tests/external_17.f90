module m
  implicit none
  private
  public :: gen, my_fun_r4, my_fun_r8

  interface gen
    module procedure gen_r4
    module procedure gen_r8
  end interface

contains

  subroutine gen_r4(f, x)
    real(4), external :: f
    real(4), intent(out) :: x
    x = f(1.0_4)
  end subroutine gen_r4

  subroutine gen_r8(f, x)
    real(8), external :: f
    real(8), intent(out) :: x
    x = f(1.0_8)
  end subroutine gen_r8

  function my_fun_r4(z) result(r)
    real(4), intent(in) :: z
    real(4) :: r
    r = z + 1.0e-4_4
  end function my_fun_r4

  function my_fun_r8(z) result(r)
    real(8), intent(in) :: z
    real(8) :: r
    r = z + 1.0e-10_8
  end function my_fun_r8

end module m

program p
  use m, only: gen, my_fun_r4, my_fun_r8
  implicit none
  real(4) :: out4
  real(8) :: out8

  call gen(my_fun_r4, out4)
!   call gen(my_fun_r8, out8)

  if (abs(out4 - (1.0_4 + 1.0e-4_4)) > 1.0e-6_4) then
    error stop "unexpected gen_r4 result"
  end if
!   if (abs(out8 - (1.0_8 + 1.0e-10_8)) > 1.0e-12_8) then
!     error stop "unexpected gen_r8 result"
!   end if
end program p
