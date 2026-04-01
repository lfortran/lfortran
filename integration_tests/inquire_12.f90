! Test INQUIRE statement with stream, round, pending, asynchronous, and iomsg keywords
program test_inquire_keywords
    implicit none
    integer :: unit_num, ios
    character(len=256) :: filename, access_mode, stream_val, round_val
    character(len=256) :: async_val, message
    logical :: exist, opened, pending_val
    
    filename = "test_inquire_file.dat"
    
    ! Create a test file
    open(newunit=unit_num, file=filename, status='replace', access='stream', action='write', iostat=ios)
    if (ios /= 0) then
        print *, "Error opening file for write"
        error stop
    end if
    write(unit_num) 123
    close(unit_num)
    
    ! Test INQUIRE with all new keywords
    print *, "Testing INQUIRE with new keywords..."
    
    ! Open file for reading
    open(newunit=unit_num, file=filename, status='old', access='stream', &
         action='read', iostat=ios)
    if (ios /= 0) then
        print *, "Error opening file for read"
        error stop
    end if
    
    ! INQUIRE with stream keyword
    inquire(unit=unit_num, access=access_mode, stream=stream_val, &
            round=round_val, pending=pending_val, asynchronous=async_val, &
            iomsg=message, iostat=ios)
    
    if (ios /= 0) then
        print *, "INQUIRE failed with iostat=", ios
        print *, "Message: ", trim(message)
        error stop
    end if
    
    print *, "Access mode: ", trim(access_mode)
    print *, "Stream: ", trim(stream_val)
    print *, "Round: ", trim(round_val)
    print *, "Pending: ", pending_val
    print *, "Asynchronous: ", trim(async_val)
    print *, "IoMsg: ", trim(message)
    
    ! Validate results
    if (trim(access_mode) /= "STREAM") then
        print *, "ERROR: Expected access='STREAM', got '", trim(access_mode), "'"
        error stop
    end if
    
    if (trim(stream_val) /= "YES") then
        print *, "ERROR: Expected stream='YES', got '", trim(stream_val), "'"
        error stop
    end if
    
    if (.not. is_valid_round(round_val)) then
        print *, "ERROR: Invalid round value: '", trim(round_val), "'"
        error stop
    end if
    
    if (pending_val) then
        print *, "ERROR: Expected pending=.false., got .true."
        error stop
    end if
    
    if (trim(async_val) /= "NO") then
        print *, "ERROR: Expected asynchronous='NO', got '", trim(async_val), "'"
        error stop
    end if
    
    close(unit_num)
    
    ! Test with sequential access
    open(newunit=unit_num, file=filename, status='old', access='sequential', &
         action='read', iostat=ios)
    if (ios /= 0) then
        print *, "Error opening file for sequential read"
        error stop
    end if
    
    stream_val = ""
    inquire(unit=unit_num, stream=stream_val, iostat=ios)
    if (trim(stream_val) /= "NO") then
        print *, "ERROR: Expected stream='NO' for sequential access, got '", trim(stream_val), "'"
        error stop
    end if
    
    close(unit_num)
    
    ! Clean up
    open(unit=10, file=filename, status='old', iostat=ios)
    if (ios == 0) then
        close(10, status='delete')
    end if
    
    print *, "All tests passed!"
    
contains
    
    logical function is_valid_round(round_str)
        character(len=*), intent(in) :: round_str
        ! Valid round values include NEAREST, UP, DOWN, ZERO, PROCESSOR_DEFINED, etc.
        is_valid_round = len_trim(round_str) > 0
    end function is_valid_round
    
end program test_inquire_keywords
