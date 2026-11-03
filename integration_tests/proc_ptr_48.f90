! A specific intrinsic function is a valid procedure pointer target even
! though it is elemental; only a nonintrinsic elemental procedure is not
! (#16573). So is a nonelemental module procedure, also as an initial target.
module proc_ptr_48_m
implicit none
abstract interface
    real function r_iface(x)
        real, intent(in) :: x
    end function
    integer function i_iface(x)
        integer, intent(in) :: x
    end function
end interface
contains
integer function inc(x)
    integer, intent(in) :: x
    inc = x + 1
end function
end module

program proc_ptr_48
use proc_ptr_48_m
implicit none
intrinsic :: sin, iabs
procedure(r_iface), pointer :: p
procedure(i_iface), pointer :: q => inc
if (q(1) /= 2) error stop
p => sin
q => iabs
if (abs(p(0.0)) > 1e-6) error stop
if (q(-3) /= 3) error stop
print *, "ok"
end program
