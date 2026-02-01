! Test for https://github.com/lfortran/lfortran/issues/8933
! Internal files with explicit unit= keyword
program read_34
    implicit none
    character(32) :: string
    integer :: ival

    ! Test internal READ with unit= keyword
    string = '1234'
    read(unit=string, fmt='(I4)') ival
    if (ival /= 1234) error stop "Internal read with unit= failed"

    ! Test internal WRITE with unit= keyword
    ival = -42
    write(unit=string, fmt='(I6)') ival
    if (adjustl(string) /= '-42') error stop "Internal write with unit= failed"

    print *, "PASSED: internal files with unit= keyword"
end program read_34
