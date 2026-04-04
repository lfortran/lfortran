! Test INQUIRE statement with pending and asynchronous keywords
program test_inquire_keywords
    implicit none
    integer :: unit_num, ios
    character(len=256) :: filename
    character(len=256) :: async_val
    logical :: pending_val
    
    filename = "test_inquire_file.dat"
    
    ! Create a test file
    open(newunit=unit_num, file=filename, status='replace', access='stream', action='write', iostat=ios)
    if (ios /= 0) then
        print *, "Error opening file for write"
        error stop
    end if
    write(unit_num) 123
    close(unit_num)
    
    ! Open file for reading
    open(newunit=unit_num, file=filename, status='old', access='stream', &
         action='read', iostat=ios)
    if (ios /= 0) then
        print *, "Error opening file for read"
        error stop
    end if
    
    ! INQUIRE with pending and asynchronous keywords
    inquire(unit=unit_num, pending=pending_val, asynchronous=async_val, &
            iostat=ios)
    
    if (ios /= 0) then
        print *, "INQUIRE failed with iostat=", ios
        error stop
    end if
    
    print *, "Pending: ", pending_val
    print *, "Asynchronous: ", trim(async_val)
    
    ! Validate results
    if (pending_val) then
        print *, "ERROR: Expected pending=.false., got .true."
        error stop
    end if
    
    if (trim(async_val) /= "NO") then
        print *, "ERROR: Expected asynchronous='NO', got '", trim(async_val), "'"
        error stop
    end if
    
    close(unit_num)
    
    ! Clean up
    open(unit=10, file=filename, status='old', iostat=ios)
    if (ios == 0) then
        close(10, status='delete')
    end if
    
    print *, "All tests passed!"
    
end program test_inquire_keywords
