! With --implicit-interface, a module references an external procedure
! defined in this file through an implicit interface when a bound of its
! dummy calls another external function of this file without an explicit
! interface, which a copy of its interface in the module cannot call.
! The function is implicitly typed. It is not a specification function
! (F2018 10.1.11), so GFortran rejects this extension and the test has no
! gfortran label.
module external_38_m
contains
    subroutine drv(a, s)
        real, intent(inout) :: a(6)
        real, intent(out) :: s
        call external_38_scale(a, 3)
        s = external_38_sum(a, 3)
    end subroutine
end module

integer function len_external_38(n)
integer, intent(in) :: n
len_external_38 = 2*n
end function

subroutine external_38_scale(a, n)
integer, intent(in) :: n
real, intent(inout) :: a(len_external_38(n))
a = a * size(a)
end subroutine

real function external_38_sum(a, n)
integer, intent(in) :: n
real, intent(in) :: a(len_external_38(n))
external_38_sum = sum(a)
end function

program external_38
use external_38_m
implicit none
real :: a(6), s
a = 1
call drv(a, s)
print *, a, s
if (any(a /= 6)) error stop 1
if (s /= 36) error stop 2
end program
