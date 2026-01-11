program file_39
    ! Test position='append' in OPEN statement
    implicit none
    integer :: u, a(3), b(3)
    character(*), parameter :: filename = "file_39_test.txt"
    
    ! Write first line
    open(newunit=u, file=filename, action='write', status='replace', &
         access='stream', form='formatted')
    write(u, *) 1, 2, 3
    close(u)
    
    ! Append second line using position='append'
    open(newunit=u, file=filename, action='write', position='append', &
         status='old', access='stream', form='formatted')
    write(u, *) 4, 5, 6
    close(u)
    
    ! Read both lines back
    open(newunit=u, file=filename, action='read', status='old', &
         access='stream', form='formatted')
    read(u, *) a
    read(u, *) b
    close(u)
    
    ! Verify first line is [1,2,3], second line is [4,5,6]
    if (.not. all(a == [1, 2, 3])) then
        print *, "FAIL: first line should be [1,2,3], got", a
        error stop
    end if
    if (.not. all(b == [4, 5, 6])) then
        print *, "FAIL: second line should be [4,5,6], got", b
        error stop
    end if
    
    print *, "PASS"
end program
