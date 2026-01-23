program test_namelist_internal_derived
    implicit none

    type :: point
        integer :: x=0
        integer :: y=0
    end type point

    type(point) :: dot
    character(len=:), allocatable :: record

    namelist /nml_dot/ dot

    record = '&nml_dot dot%x=1, dot%y=2 /'

    read(record, nml=nml_dot)

    if (dot%x /= 1) error stop
    if (dot%y /= 2) error stop
end program test_namelist_internal_derived
