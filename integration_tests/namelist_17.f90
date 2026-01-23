program test_namelist_derived_types
    implicit none
    integer :: a, b
    real :: c
    type :: point
        integer :: x, y
    end type
    type(point) :: p

    namelist /nml/ a, b, c, p

    a = 1
    b = 2
    c = 3.5
    p%x = 4
    p%y = 5

    open(unit=10, file='namelist_dt_17.dat', status='replace', form='formatted')
    write(10, nml=nml)
    close(10)

    a = 0
    b = 0
    c = 0
    p%x = 0
    p%y = 0

    open(unit=10, file='namelist_dt_17.dat', status='old', form='formatted')
    read(10, nml=nml)
    close(10)

    print *, a
    if (a /= 1) error stop
    print *, b
    if (b /= 2) error stop
    print *, c
    if (abs(c-3.5) > 1e-5) error stop
    print *, p%x
    if (p%x /= 4) error stop
    print *, p%y
    if (p%y /= 5) error stop
end program test_namelist_derived_types
