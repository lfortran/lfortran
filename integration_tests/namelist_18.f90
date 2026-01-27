program test_namelist_derived_nested
    implicit none

    type :: inner_t
        integer :: vec(2)
        real :: mat(2, 2)
    end type

    type :: outer_t
        type(inner_t) :: inner
        integer :: tag
        real :: arr(3)
    end type

    type(outer_t) :: o

    namelist /nml/ o

    o%inner%vec = [10, 20]
    o%inner%mat = reshape([1.0, 2.0, 3.0, 4.0], [2, 2])
    o%tag = 7
    o%arr = [5.5, 6.5, 7.5]

    open(unit=10, file='namelist_dt_18.dat', status='replace', form='formatted')
    write(10, nml=nml)
    close(10)

    o%inner%vec = 0
    o%inner%mat = 0.0
    o%tag = 0
    o%arr = 0.0

    open(unit=10, file='namelist_dt_18.dat', status='old', form='formatted')
    read(10, nml=nml)
    close(10)

    if (any(o%inner%vec /= [10, 20])) error stop
    if (any(abs(o%inner%mat - reshape([1.0, 2.0, 3.0, 4.0], [2, 2])) > 1.0e-5)) error stop
    if (o%tag /= 7) error stop
    if (any(abs(o%arr - [5.5, 6.5, 7.5]) > 1.0e-5)) error stop
end program test_namelist_derived_nested
