program open_recl_test
    integer :: u
    open(newunit=u, file="test.bin", access="direct", recl=4)
    close(u)
end program