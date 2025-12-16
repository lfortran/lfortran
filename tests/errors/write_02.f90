program write_02
    implicit none

    character(len=*), parameter :: temp_file = "lfortran_temporary_file_write_02.dat"
    character(len=*), parameter :: test_line = "HelloWorld!"
    integer :: unit, ios
    ios = 0
    unit = 1

    open(newunit=unit, form="unformatted", file=temp_file) !unformatted
    write(unit,*) test_line !formatted
    close(unit)
end program