! Passing an assumed-size CHARACTER(LEN=1) array to a
! BIND(C) procedure whose dummy is CHARACTER(KIND=C_CHAR) :: op(*)
! (StringArraySinglePointer). Reduced from netcdf-fortran
! (fortran/nf_attio.F90, nf_put_att_text_a -> nc_put_att_text).
module bindc_57_interfaces
  use iso_c_binding, only: c_int, c_char
  implicit none
  interface
    function nc_sum_att_text(op, n) bind(c, name="nc_sum_att_text") result(status)
      import :: c_int, c_char
      character(kind=c_char), intent(in) :: op(*)
      integer(c_int), value :: n
      integer(c_int) :: status
    end function
  end interface
contains
  integer function nf_put_att_text_a(text, n) result(status)
    character(len=1), intent(in) :: text(*)
    integer, intent(in) :: n
    integer(c_int) :: cstatus
    ! The Fortran string descriptor is converted to the C data pointer.
    cstatus = nc_sum_att_text(text, int(n, c_int))
    status = cstatus
  end function
end module

program bindc_57
  use bindc_57_interfaces, only: nf_put_att_text_a
  implicit none
  character(len=1) :: buf(3)
  integer :: r
  buf(1) = 'a'
  buf(2) = 'b'
  buf(3) = 'c'
  r = nf_put_att_text_a(buf, 3)
  print *, "sum of character codes =", r
  ! 'a' + 'b' + 'c' = 97 + 98 + 99 = 294 (verifies contiguous data passing)
  if (r /= 294) error stop
end program bindc_57
