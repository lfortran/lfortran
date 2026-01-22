program test_namelist_iolist
    implicit none

    type :: point
        integer :: x=0
        integer :: y=0
    end type point

    type(point) :: dot

    namelist /nml_dot/ dot

    dot%x = 10
    dot%y = 20

    write(*, nml_dot)

    open(unit=10, file='namelist_21.dat', status='replace', form='formatted')
    write(10, nml_dot)
    close(10)

    dot%x = 0
    dot%y = 0

    open(unit=10, file='namelist_21.dat', status='old', form='formatted')
    read(10, nml_dot)
    close(10)

    if (dot%x /= 10) error stop
    if (dot%y /= 20) error stop
end program test_namelist_iolist
