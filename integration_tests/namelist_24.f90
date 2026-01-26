module namelist_24_mod
    implicit none
    integer :: a = 0
end module namelist_24_mod

program test_namelist_external_symbol
    use namelist_24_mod
    implicit none

    namelist /nml/ a

    a = 42
    open(unit=10, file='namelist_24.dat', status='replace', form='formatted')
    write(10, nml=nml)
    close(10)

    a = 0
    open(unit=10, file='namelist_24.dat', status='old', form='formatted')
    read(10, nml=nml)
    close(10)

    if (a /= 42) error stop "Namelist external symbol test failed"
    print *, "Namelist external symbol test passed!"
end program test_namelist_external_symbol
