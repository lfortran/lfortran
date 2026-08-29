! The coarray pass synthesizes declarations of the `prif` module procedures it
! calls into (Caffeine's coarray runtime) directly in the global scope, where
! they have no ASR owner. Those procedures are Fortran module procedures with an
! explicit interface, so their CHARACTER dummies (prif_stop's stop_code_char,
! prif_sync_all's errmsg) keep the string-descriptor ABI.
!
! Regression test: the synthesized declarations were mistaken for ownerless
! external procedures and given the classic hidden-length CHARACTER ABI (the
! data pointer at the argument position plus a trailing length). A caller
! reaching the same procedure through `use prif` keeps the descriptor ABI, so
! the two disagreed on the argument list of one LLVM function and the module
! failed verification with "Incorrect number of arguments passed to called
! function!" (Caffeine's example/hello.F90).
program coarray_prif_char_abi_01
  implicit none
  integer :: n
  n = 1
  sync all
  print *, n
end program coarray_prif_char_abi_01
