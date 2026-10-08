! file partstring.f90
program read_substring_unit_666
    implicit none
    character(10) :: string = 'ABCDEFGHIJ'
    open(666, file='read_substring_unit_666.f90')
    read(666, '(A)') string(1:6)
    write(*, '(A)') string
    if (string /= '! fileGHIJ') error stop
    close(666)
end program read_substring_unit_666
