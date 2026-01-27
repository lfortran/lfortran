program namelist_internal_read_array
    implicit none

    integer :: a, b, c
    real :: x
    character(len=20) :: name
    character(len=256) :: readme(7)

    namelist /EXPECTED/ a, b, name, x, c

    ! Initialize with default values
    a = 0
    b = 0
    c = 0
    x = 0.0
    name = ''

    ! Build the namelist input
    readme(1) = '&EXPECTED'
    readme(2) = ' a = 42'
    readme(3) = ' b = 100'
    readme(4) = ' name = "Hello"'
    readme(5) = ' x = 3.14'
    readme(6) = ' c = -7'
    readme(7) = '/'

    read(readme, nml=EXPECTED)

    if (a /= 42) error stop "a has wrong value"
    if (b /= 100) error stop "b has wrong value"
    if (c /= -7) error stop "c has wrong value"
    if (abs(x - 3.14) > 1.0e-5) error stop "x has wrong value"
    if (trim(name) /= "Hello") error stop "name has wrong value"

    print *, 'a =', a
    print *, 'b =', b
    print *, 'c =', c
    print *, 'x =', x
    print *, 'name = "' // trim(name) // '"'
    print *, 'All tests passed!'
end program namelist_internal_read_array
