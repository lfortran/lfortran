subroutine mysub (nint, in, iout)
iout = nint (in)
end subroutine

program implicit_interface_30

integer :: ival
intrinsic :: iabs

call mysub (iabs, -42, ival)
if (ival /= 42) error stop

end program