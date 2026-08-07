! Separately-compiled external subroutine with a CHARACTER(len=*) dummy,
! mirroring netcdf-fortran's nf_get_vlen_element wrapper. It reads the first
! byte of its dummy (the crash site before the ABI fix) and writes it back.
subroutine get_first_byte(vlen_element)
  implicit none
  character(len=*), intent(inout) :: vlen_element
  if (ichar(vlen_element(1:1)) /= 65) error stop
  vlen_element(1:1) = 'Z'
end subroutine get_first_byte
