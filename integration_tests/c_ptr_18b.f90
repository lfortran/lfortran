! External helper for c_ptr_18, compiled separately and called through an
! implicit interface. Takes the C_LOC() of an assumed-size
! CHARACTER(KIND=C_CHAR),TARGET dummy that, via storage association, actually
! receives a REAL array from the caller.
!
! Only the data pointer is used. The caller passes a REAL actual through an
! implicit interface, so no hidden CHARACTER length accompanies it (as with
! gfortran); this procedure must therefore never call LEN(values).
subroutine store_ptr(values, cptr)
  use iso_c_binding, only: c_char, c_ptr, c_loc
  character(kind=c_char), intent(in), target :: values(*)
  type(c_ptr), intent(out) :: cptr
  cptr = c_loc(values)
end subroutine store_ptr
