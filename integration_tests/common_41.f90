subroutine dintdy()
    integer :: iownd
    double precision :: rowns
    common /dls001/ rowns(209), iownd(6)
    return
end subroutine dintdy

subroutine dstoda()
    integer :: ialth
    double precision :: conit, crate, el, elco, hold, rmax, tesco
    common /dls001/ conit, crate, el(13), elco(13,12), hold, rmax, tesco(3,12), ialth
    ialth = 2
    if (ialth == 1) ialth = 2
    if (ialth /= 2) error stop
    return
end subroutine dstoda

program common_41
    call dstoda()
end program common_41
