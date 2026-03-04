program intrinsics_421
    implicit none
    integer, parameter :: dp = kind(1d0)
    integer, parameter :: nextp = selected_real_kind(precision(1d0) + 1)
    integer, parameter :: ep = merge(nextp, dp, nextp > 0)

    if (ep /= 16) error stop
    if (precision(1.0_ep) /= 33) error stop

    print *, 'pi_dp=', acos(-1.0_dp)
    print *, 'pi_ep=', acos(-1.0_ep)
end program intrinsics_421
