! Module procedures call external procedures defined in the same file, before
! and after the module, with no interface for them: with a scalar, with a
! whole array sequence associated with a dummy of another rank (the callee
! contains a procedure), and from a contained procedure.
subroutine external_32_twice(x)
implicit none
real(8), intent(inout) :: x
x = 2*x
end subroutine

module external_32_mod
implicit none
contains
    subroutine scale_scalar(x)
    real(8), intent(inout) :: x
    call external_32_twice(x)
    call external_32_thrice(x)
    end subroutine

    subroutine scale_matrix(n, c)
    integer, intent(in) :: n
    real, intent(inout) :: c(6)
    call external_32_matrix(n, c)
    end subroutine

    subroutine scale_inner(x)
    real(8), intent(inout) :: x
    call inner()
    contains
        subroutine inner()
        call external_32_twice(x)
        end subroutine
    end subroutine
end module

program external_32
use external_32_mod
implicit none
real(8) :: y
real :: c(6)
y = 1
call scale_scalar(y)
print *, y
if (abs(y - 6d0) > 1d-12) error stop 1
c = 1
call scale_matrix(2, c)
print '(6F4.1)', c
if (any(abs(c - [2, 2, 2, 2, 1, 1]) > 1e-6)) error stop 2
call scale_inner(y)
print *, y
if (abs(y - 12d0) > 1d-12) error stop 3
end program

subroutine external_32_thrice(x)
implicit none
real(8), intent(inout) :: x
x = 3*x
end subroutine

subroutine external_32_matrix(n, x)
implicit none
integer, intent(in) :: n
real, intent(inout) :: x(n,2)
call double_all()
contains
    subroutine double_all()
    x = 2*x
    end subroutine
end subroutine
