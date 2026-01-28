program equivalence_16
    ! Test EQUIVALENCE with -fdefault-integer-8 (ILP64 mode)
    ! Verifies descriptor fields use correct index type width
    implicit none
    integer :: iwork(10)
    double precision :: rwork(5)
    equivalence (iwork(1), rwork(1))

    rwork(1) = 42.0d0

    ! With ILP64, integer is 8 bytes, same as double precision.
    ! iwork(1) overlaps rwork(1) exactly, so reinterpreting the
    ! bit pattern of 42.0d0 as integer(8) must give 4631107791820423168.
    if (iwork(1) /= transfer(42.0d0, iwork(1))) error stop

    ! Write through integer view and verify via real view
    iwork(1) = transfer(99.0d0, iwork(1))
    if (rwork(1) /= 99.0d0) error stop
end program
