program intrinsics_409

    implicit none
    character(len=100) :: output
    integer :: iostat, unit_num
    
    open(42, file='test_exec_flush.txt', position='rewind')
    write(42, '(A)') 'Hello from LFortran'

    call execute_command_line('cat test_exec_flush.txt > test_exec_output.txt')
    
    close(42)
    
    open(unit=43, file='test_exec_output.txt', status='old', iostat=iostat)
    if (iostat /= 0) error stop 'Failed to open output file'
    
    read(43, '(A)', iostat=iostat) output
    if (iostat /= 0) error stop 'Failed to read output file'
    close(43)
    
    if (trim(output) /= 'Hello from LFortran') then
        print *, 'Expected: Hello from LFortran'
        print *, 'Got: ', trim(output)
        error stop 'Output mismatch'
    end if
    
    open(unit=44, file='test_exec_flush.txt', status='old')
    close(44, status='delete')
    open(unit=45, file='test_exec_output.txt', status='old')
    close(45, status='delete')
    
    print *, 'PASSED'
end program intrinsics_409
