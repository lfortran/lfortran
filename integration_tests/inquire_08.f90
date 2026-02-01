program inquire_08
    implicit none
    integer :: ioerr, nxtrec, recl, unit_no

    inquire (iolength=recl) 42, 42.0, 'xyzzy'
    if (recl /= 13) error stop "iolength wrong"

    ioerr = -42
    open (newunit=unit_no, file='test_inquire.dat', status='replace', &
          access='direct', recl=recl, form='unformatted', iostat=ioerr)
    if (ioerr /= 0) error stop "open failed"

    ioerr = -42
    nxtrec = -43
    inquire (unit_no, iostat=ioerr, nextrec=nxtrec)
    if (ioerr /= 0) error stop "iostat should be 0"
    if (nxtrec /= 1) error stop "nextrec should be 1"

    close (unit_no, status='delete')
    print *, "All tests passed"
end program
