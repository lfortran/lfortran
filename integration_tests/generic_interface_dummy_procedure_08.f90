module generic_interface_dummy_procedure_08_mod
implicit none
private
public :: gen, gen_res, fun_r4, fun_r8, gen_sub, gen_untyped, sub_r8, &
    sub_int, called

integer :: called = 0

abstract interface
    subroutine sub_iface(z)
        real(8), intent(in) :: z
    end subroutine
end interface

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

! A typed external dummy is a function, so a subroutine actual must select
! the specific with the subroutine dummy
interface gen_sub
    module procedure sub_typed_ext
    module procedure sub_explicit
end interface

! Specifics with untyped external dummies, distinguished by `x`
interface gen_untyped
    module procedure untyped_int
    module procedure untyped_real
end interface

contains

subroutine sub_typed_ext(f)
    real(8), external :: f
    called = -1
end subroutine

subroutine sub_explicit(f)
    procedure(sub_iface) :: f
    call f(1.5_8)
end subroutine

subroutine untyped_int(f, x)
    external :: f
    integer, intent(in) :: x
    call f(x)
end subroutine

subroutine untyped_real(f, x)
    external :: f
    real, intent(in) :: x
    called = -2
end subroutine

subroutine sub_r8(z)
    real(8), intent(in) :: z
    if (abs(z - 1.5_8) > 1e-12_8) error stop 7
    called = 1
end subroutine

subroutine sub_int(n)
    integer, intent(in) :: n
    called = n
end subroutine

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

called = 0
call gen_sub(sub_r8)
print *, called
if (called /= 1) error stop 8

called = 0
call gen_untyped(sub_int, 5)
print *, called
if (called /= 5) error stop 9
end program
