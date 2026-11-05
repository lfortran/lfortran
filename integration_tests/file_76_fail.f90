program file_76_fail
    ! Without IOSTAT=, POS= on a sequential unit is a runtime error.
    integer :: u
    open(newunit=u, file='file_76_fail.dat', status='replace', &
         access='sequential', form='formatted')
    write(u, '(a)') 'aaaa'
    write(u, '(a)', pos=1) 'q'
    print *, 'WRITE with POS= was accepted'
end program
