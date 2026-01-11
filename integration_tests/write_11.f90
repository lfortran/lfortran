program write_11
    implicit none

    character(len=*), parameter :: temp_file = "_lfortran_temporary_file"
    character(len=*), parameter :: test_line = "HelloWorld!"
    integer :: unit, ios
    ios = 0
    unit = 1

    open(newunit=unit, file=temp_file)
    write(unit, iostat=ios) test_line
    close(unit)
    print *, ios
    if(ios <= 0) error stop "IOS should be positive -- as a runtime error should occur"

end program