program c_ptr_19
use lfortran_intrinsic_fake, only: c_ptr, c_funptr, c_null_ptr, c_null_funptr
implicit none

type(c_ptr) :: p
type(c_funptr) :: fp

p = c_null_ptr
fp = c_null_funptr

if (p%x /= 17) error stop "fake c_ptr"
if (fp%x /= 23) error stop "fake c_funptr"
end program
