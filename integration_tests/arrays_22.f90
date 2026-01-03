program arrays_22
    use iso_fortran_env, only: wp => real64

    implicit none
    real(wp), dimension(3), parameter :: dpmpar = [epsilon(1.0_wp), &
                                                    tiny(1.0_wp), &
                                                    huge(1.0_wp)]

    real(wp), parameter :: epsmch = dpmpar(1)
    print *, epsmch
    if (abs(epsmch - epsilon(1.0_wp))/epsilon(1.0_wp) > 1e-15_wp) error stop
end program
