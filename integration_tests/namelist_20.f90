program test_namelist_internal_derived_list
    implicit none

    type :: point
        integer :: x=0
        integer :: y=0
    end type point

    type(point) :: dot
    character(len=:), allocatable :: line

    namelist /nml_dot/ dot

    line = '&nml_dot dot=1,2 /'
    read(line, nml=nml_dot)

    if (dot%x /= 1) error stop
    if (dot%y /= 2) error stop
end program test_namelist_internal_derived_list
