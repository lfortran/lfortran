module namelist_31_mod
    implicit none
    integer :: value = 11
    namelist /settings/ value
end module namelist_31_mod

program namelist_31
    use namelist_31_mod, only: settings, value
    implicit none

    open(unit=10, file='namelist_31.dat', status='replace', form='formatted')
    write(10, nml=settings)
    close(10)

    value = 0

    open(unit=10, file='namelist_31.dat', status='old', form='formatted')
    read(10, nml=settings)
    close(10)

    if (value /= 11) error stop "use only namelist failed"
    print *, "use only namelist passed"
end program namelist_31
