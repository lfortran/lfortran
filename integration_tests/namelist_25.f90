program namelist_internal_read_array
    implicit none

    integer :: a
    character(len=20) :: test
    character(len=256) :: readme(3)

    namelist /EXPECTED/ a

    test = 'a = 42'

    readme(1) = '&EXPECTED'
    readme(2) = ' ' // test
    readme(3) = '/'

    read(readme, nml=EXPECTED)

    if (a /= 42) error stop
    print *, 'a =', a
end program namelist_internal_read_array
