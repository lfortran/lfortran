program format_120
    implicit none
    integer :: i, u
    open(newunit=u, file='format_120_tmp.txt', status='replace')
    do i = 1, 300000
        write(u, '(es24.16)') 1.0
    end do
    do i = 1, 300000
        write(u, *) 1.0
    end do
    close(u, status='delete')
end program format_120
