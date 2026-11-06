! A stand-in for something that changes a VOLATILE variable by means the
! program does not show. The caller of the procedure saves the address of the
! variable here, and noalias_dummy_06_complete writes through it. It is
! compiled on its own, so the procedure that calls it cannot see what it does.
module noalias_dummy_06_pending
use iso_c_binding, only: c_ptr, c_null_ptr
implicit none
type(c_ptr) :: pending_buffer = c_null_ptr
end module

subroutine noalias_dummy_06_complete(k)
use iso_c_binding, only: c_f_pointer
use noalias_dummy_06_pending, only: pending_buffer
implicit none
integer, intent(in) :: k
real, pointer :: p(:)
call c_f_pointer(pending_buffer, p, [1])
p(1) = real(k)
end subroutine
