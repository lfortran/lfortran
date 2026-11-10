! A local procedure pointer of an internal procedure takes its interface
! from a sibling internal procedure defined after it, which has the name of a
! specific intrinsic function.
module procedure_pointer_51_mod
implicit none
contains
subroutine host(res)
    integer, intent(out) :: res
    call use_later_internal(1.0, res)
contains
    subroutine use_later_internal(x, res)
        real, intent(in) :: x
        integer, intent(out) :: res
        procedure(cos), pointer :: f
        f => cos
        res = f(x)
    end subroutine use_later_internal
    integer function cos(x)
        real, intent(in) :: x
        cos = int(x) + 41
    end function cos
end subroutine host
end module
program procedure_pointer_51
use procedure_pointer_51_mod
integer :: r
call host(r)
if (r /= 42) error stop
print *, r
end program procedure_pointer_51
