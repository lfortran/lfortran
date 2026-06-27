module read_97_mod
contains
    subroutine read_assumed_shape(unit, values)
        integer, intent(in) :: unit
        real, intent(out) :: values(:)

        read(unit, *) values
    end subroutine
end module

program read_97
    use read_97_mod, only: read_assumed_shape
    implicit none

    integer :: unit
    real :: values(3)

    unit = 21
    open(unit, file="read_97_input.txt", status="replace", action="write")
    write(unit, *) 1.0, 2.0, 3.0
    close(unit)

    open(unit, file="read_97_input.txt", status="old", action="read")
    call read_assumed_shape(unit, values)
    close(unit)

    if (any(abs(values - [1.0, 2.0, 3.0]) > 1.e-6)) error stop 1
    print *, "assumed-shape read ok"
end program
