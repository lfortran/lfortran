! Separately-compiled external subroutine with a CHARACTER(len=*) dummy,
! mirroring netcdf-fortran's nf_get_vlen_element wrapper. It reads the first
! byte of its dummy (the crash site before the ABI fix) and writes it back.
!
! It deliberately never calls LEN(): the actual argument in the caller is an
! INTEGER array, and for a non-CHARACTER actual reached through an implicit
! interface neither gfortran nor LFortran can know a length to pass, so no
! hidden length is emitted and LEN() here would read a garbage register. The
! same-file case, where the definition is visible and the length therefore *is*
! delivered, is covered by implicit_interface_59.
subroutine get_first_byte(vlen_element)
  implicit none
  character(len=*), intent(inout) :: vlen_element
  if (ichar(vlen_element(1:1)) /= 65) error stop
  vlen_element(1:1) = 'Z'
end subroutine get_first_byte
