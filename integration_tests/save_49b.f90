! Used by save_49.f90.
module save_49_m
implicit none
contains
subroutine save_49_apply(f, y)
interface
    real function f(x)
    real, intent(in) :: x
    end function
end interface
real, intent(inout) :: y
y = f(y)
end subroutine

subroutine save_49_h(a, s)
real, intent(in) :: a(:)
real, intent(out) :: s
real :: c
c = sum(a)
s = 1
call save_49_apply(inner, s)
contains
    real function inner(x)
    real, intent(in) :: x
    inner = x + c
    end function
end subroutine
end module
