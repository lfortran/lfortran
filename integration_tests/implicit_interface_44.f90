! Calling an implicit-interface external procedure with different argument
! types at different call sites must be accepted. This mirrors netcdf-fortran's
! v2 API `ncagt`, which is called first with a byte (integer(1)) array and then
! with a short (integer(2)) array. Standard Fortran performs no argument
! checking for implicit-interface externals, so the second call must not be
! rejected as a type mismatch (LFortran previously inferred the dummy type from
! the first call and wrongly rejected the second).
program implicit_interface_44
    implicit none
    integer(1) :: bytval(3)
    integer(2) :: shval(3)

    bytval = 0_1
    shval = 0_2

    ! First reference infers the (byte) interface for `store_first`.
    call store_first(bytval)
    ! Second reference passes a different type to the same external.
    call store_first(shval)

    if (bytval(1) /= 7) error stop
    print *, "ok"
end program
