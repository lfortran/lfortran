program extern_fn2
! Test that procedure arguments can shadow intrinsic function names
! when using --implicit-interface flag
! Compile with: --implicit-typing and --implicit-interface

  integer :: ival
  intrinsic :: iabs

  call mysub (iabs, -42, ival)
  if (ival /= 42) error stop "Test failed: ival should be 42"
  print *, "Test passed"

end program

subroutine mysub (nint, in, iout)
! nint is a dummy argument (receives iabs function)
! It shadows the intrinsic NINT function
! When called as nint(in), it should invoke the passed-in function, not the intrinsic

  iout = nint (in)

end subroutine
