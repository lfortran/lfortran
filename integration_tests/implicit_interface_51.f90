! Passing a non-character actual argument to a CHARACTER(len=*) dummy through
! an implicit interface is a legal F77 storage-association idiom used by
! netcdf-fortran (nf_test4/ftst_vars4.F calls nf_get_vlen_element, whose
! `vlen_element` dummy is CHARACTER(len=*), with an integer*8 array actual).
!
! The external procedure is compiled separately (see implicit_interface_51b.f90)
! so the caller sees no explicit interface. The classic Fortran ABI passes the
! actual's address as the character data pointer and the length as a hidden
! trailing argument. LFortran previously passed a { data, len } string
! descriptor, so the callee reinterpreted the first two integer words as the
! descriptor and dereferenced a bogus data pointer, crashing at run time.
!
! This test assumes a little-endian target (as do the CI platforms): the least
! significant byte of buf(1) aliases the first character of the dummy.
program implicit_interface_51
  implicit none
  integer(8) :: buf(4)
  external get_first_byte
  integer :: i
  do i = 1, 4
     buf(i) = 0
  end do
  buf(1) = 65     ! low byte is 'A'
  call get_first_byte(buf)
  ! The callee overwrote the first character with 'Z' (code 90) in place.
  if (iand(buf(1), 255_8) /= 90) error stop
  ! The remaining bytes of buf(1) and the other elements must be untouched.
  if (buf(1) /= 90) error stop
  if (buf(2) /= 0) error stop
  if (buf(3) /= 0) error stop
  if (buf(4) /= 0) error stop
  print *, "OK"
end program implicit_interface_51
