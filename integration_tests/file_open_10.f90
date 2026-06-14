program file_open_10
    implicit none
    integer :: u
    integer :: ios
    character(len=100) :: line
    
    open(newunit=u, file="test_read_access.txt", status="replace", action="write")
    write(u, '(A)') "Hello, world!"
    
    ! Attempt to read from a write-only file
    read(u, '(A)', iostat=ios) line
    
    if (ios /= 5007) then
        print *, "Error: Expected iostat=5007 for reading write-only file, got ", ios
        error stop
    end if
    
    close(u)
    
    ! Clean up
    open(newunit=u, file="test_read_access.txt", status="old")
    close(u, status="delete")
end program file_open_10
