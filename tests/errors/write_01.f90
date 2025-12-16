program write_01
    implicit none

    character(len=*), parameter :: temp_file = "lfortran_temporary_file_write_01.dat"
    character(len=*), parameter :: test_line = "HelloWorld!"
    integer :: unit, ios
    ios = 0
    unit = 1

    open(newunit=unit, file=temp_file) !formatted
    write(unit) test_line !unformatted
    close(unit)
end program