program cast_test
    logical :: arr(2) 
    open(10, file='read_34.txt', access='direct', recl=4, status='replace')
    write(10, rec=1) .true.
    arr = .false.
    ! This read requires a cast for read to work correctly
    read(10, rec=1) arr(1)
    if (.not. arr(1)) error stop
    close(10, status='delete')
end program