! Forwarding an assumed-size CHARACTER(KIND=C_CHAR) value(*) dummy of an
! external procedure (implicit interface) to a BIND(C) procedure. The external
! wrapper get_member_value receives an INTEGER actual argument through storage
! association and must forward the bare data pointer (not a string descriptor)
! to the C callee. Reduced from netcdf-fortran (nf_inq_enum_member ->
! nc_inq_enum_member). LFortran used to dereference the argument as a string
! descriptor and pass a garbage pointer to the BIND(C) callee, crashing at
! runtime.
program bindc_59
    implicit none
    integer :: member_value
    member_value = -1
    ! get_member_value is external (no explicit interface); the integer actual
    ! is storage-associated with the wrapper's CHARACTER(KIND=C_CHAR) dummy.
    call get_member_value(member_value)
    print *, "member_value =", member_value
    if (member_value /= 42) error stop
end program bindc_59
