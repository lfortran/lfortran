program file_open_10
    implicit none
    integer :: u
    integer :: ios
    character(len=100) :: line
    character(len=5) :: words(2)
    
    open(newunit=u, file="test_read_access.txt", status="replace", action="write")
    write(u, '(A)') "Hello, world!"
    
    ! Attempt to read from a write-only file
    read(u, '(A)', iostat=ios) line
    
    if (ios /= 5007) then
        print *, "Error: Expected iostat=5007 for reading write-only file, got ", ios
        error stop
    end if
    
    close(u)

    open(newunit=u, file="test_read_access_array.txt", status="replace", action="write")
    write(u, '(A)') "Hello, world!"

    ! Attempt to read a character array from a write-only file
    read(u, *, iostat=ios) words

    if (ios /= 5007) then
        print *, "Error: Expected iostat=5007 for reading character array from write-only file, got ", ios
        error stop
    end if

    close(u)

    open(newunit=u, file="test_empty_read_access.txt", status="replace", action="write")
    write(u, '(A)') "Hello, world!"

    ! Attempt an empty READ statement on a write-only file
    read(u, *, iostat=ios)

    if (ios /= 5007) then
        print *, "Error: Expected iostat=5007 for empty read on write-only file, got ", ios
        error stop
    end if

    close(u)
    
    ! Clean up
    open(newunit=u, file="test_read_access.txt", status="old")
    close(u, status="delete")
    open(newunit=u, file="test_read_access_array.txt", status="old")
    close(u, status="delete")
    open(newunit=u, file="test_empty_read_access.txt", status="old")
    close(u, status="delete")
end program file_open_10
