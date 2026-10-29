! A stand-in for a pending asynchronous operation, such as an MPI_Irecv, that
! a call such as MPI_Wait completes without the buffer as an argument. The
! caller of the procedure saves the address of the buffer here, and
! noalias_dummy_05_complete writes through it. It is compiled on its own, so
! the procedure that calls it cannot see what it does.
module noalias_dummy_05_pending
use iso_c_binding, only: c_ptr, c_null_ptr
implicit none
type(c_ptr) :: pending_buffer = c_null_ptr
end module

subroutine noalias_dummy_05_complete(k)
use iso_c_binding, only: c_f_pointer
use noalias_dummy_05_pending, only: pending_buffer
implicit none
integer, intent(in) :: k
real, pointer :: p(:)
call c_f_pointer(pending_buffer, p, [1])
p(1) = real(k)
end subroutine
