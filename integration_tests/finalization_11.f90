! Defined operations on a finalizable function result, in a procedure whose
! ASR is read from a module file: the function is called once per reference,
! and its result is finalized after the statement, not on return.
program finalization_11
use finalization_11_module
implicit none
call run()
print *, ncalls, nfin
if (ncalls /= 0) error stop 4
if (nfin /= 0) error stop 5
end program
