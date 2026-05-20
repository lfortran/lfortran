program rewind_02
    ! Test that WRITE implicitly opens a unit and REWIND works on it
    implicit none
    integer :: x

    ! Write to unit 10 without explicit OPEN (implicit open)
    write(10, *) 42

    ! Rewind should succeed on the implicitly opened unit
    rewind(10)

    ! Read back the value
    read(10, *) x
    if (x /= 42) error stop

    close(10, status="delete")
    print *, "PASS"
end program rewind_02
