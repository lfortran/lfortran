! A stand-in for a pending asynchronous operation, such as a persistent
! MPI_Send_init request that MPI_Start runs without the buffer as an
! argument. The caller saves the address of the buffer here, and
! dead_array_store_03_send reads through it. It is compiled on its own, so
! the procedure that calls it cannot see what it does.
module dead_array_store_03_pending
use iso_c_binding, only: c_ptr, c_null_ptr
implicit none
type(c_ptr) :: pending_buffer = c_null_ptr
real :: sent = -1
end module

subroutine dead_array_store_03_send()
use iso_c_binding, only: c_f_pointer
use dead_array_store_03_pending, only: pending_buffer, sent
implicit none
real, pointer :: p(:)
call c_f_pointer(pending_buffer, p, [1])
sent = p(1)
end subroutine
