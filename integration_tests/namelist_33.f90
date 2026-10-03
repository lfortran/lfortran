program namelist_33
    implicit none

    real :: value
    namelist /settings/ value

    value = 1.0
    print settings
end program namelist_33
