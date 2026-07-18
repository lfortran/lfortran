! Test C_LOC() of an assumed-size CHARACTER(KIND=C_CHAR),TARGET dummy argument
! that receives a numeric (REAL) actual argument through an implicit (external)
! interface. The assumed-size character dummy must be passed as a bare data
! pointer (storage association), like a numeric assumed-size array, so that
! C_LOC returns the address of the raw data instead of dereferencing the data
! as an array/string descriptor (which crashed at runtime).
!
! Reduced from netcdf-fortran (nf_fortv2.F90): cvaluesptr = C_LOC(values)
! where a REAL array is passed to CHARACTER(KIND=C_CHAR),TARGET :: values(*).

subroutine store_ptr(values, cptr)
  use iso_c_binding, only: c_char, c_ptr, c_loc
  character(kind=c_char), intent(in), target :: values(*)
  type(c_ptr), intent(out) :: cptr
  cptr = c_loc(values)
end subroutine store_ptr

program c_ptr_18
  use iso_c_binding, only: c_ptr, c_loc, c_f_pointer, c_associated
  implicit none
  external :: store_ptr
  real, target :: array(3)
  type(c_ptr) :: cptr
  real, pointer :: rp(:)

  array = [1.5, 2.5, 3.5]

  call store_ptr(array, cptr)

  ! C_LOC of the assumed-size character dummy must be the address of the
  ! passed REAL data buffer, not a dereferenced (garbage) pointer.
  if (.not. c_associated(cptr)) error stop "c_loc returned null"
  if (.not. c_associated(cptr, c_loc(array))) error stop "c_loc address mismatch"

  call c_f_pointer(cptr, rp, [3])
  if (abs(rp(1) - 1.5) > 1e-6) error stop "wrong value 1"
  if (abs(rp(2) - 2.5) > 1e-6) error stop "wrong value 2"
  if (abs(rp(3) - 3.5) > 1e-6) error stop "wrong value 3"

  print *, "ok"
end program c_ptr_18
