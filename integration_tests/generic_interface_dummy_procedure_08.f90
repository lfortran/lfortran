module generic_interface_dummy_procedure_08_mod
implicit none
private
public :: gen, gen_res, fun_r4, fun_r8

! Specifics distinguished by the data argument `x`
interface gen
    module procedure gen_r4
    module procedure gen_r8
end interface

! Specifics distinguished only by the result type of the typed external
! dummy function `f`
interface gen_res
    module procedure res_r4
    module procedure res_r8
end interface

contains

subroutine gen_r4(f, x)
    real(4), external :: f
    real(4), intent(out) :: x
    x = f(1.0_4)
end subroutine

subroutine gen_r8(f, x)
    real(8), external :: f
    real(8), intent(out) :: x
    x = f(1.0_8)
end subroutine

integer function res_r4(f)
    real(4), external :: f
    if (abs(f(1.0_4) - 3.0_4) > 1e-6_4) error stop 1
    res_r4 = 4
end function

integer function res_r8(f)
    real(8), external :: f
    if (abs(f(1.0_8) - 2.0_8) > 1e-12_8) error stop 2
    res_r8 = 8
end function

function fun_r4(z) result(r)
    real(4), intent(in) :: z
    real(4) :: r
    r = z * 3.0_4
end function

function fun_r8(z) result(r)
    real(8), intent(in) :: z
    real(8) :: r
    r = z * 2.0_8
end function

end module

program generic_interface_dummy_procedure_08
use generic_interface_dummy_procedure_08_mod
implicit none
real(4) :: out4
real(8) :: out8

call gen(fun_r8, out8)
print *, out8
if (abs(out8 - 2.0_8) > 1e-12_8) error stop 3

call gen(fun_r4, out4)
print *, out4
if (abs(out4 - 3.0_4) > 1e-6_4) error stop 4

if (gen_res(fun_r4) /= 4) error stop 5
if (gen_res(fun_r8) /= 8) error stop 6
print *, gen_res(fun_r4), gen_res(fun_r8)
end program
