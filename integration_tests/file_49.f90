! Test for https://github.com/lfortran/lfortran/issues/4647
! Scratch file with form='unformatted'
! Exact MRE from issue body
program file_49
    implicit none
    integer :: unit_no
    character :: q*12
    open(newunit=unit_no, status='scratch', form='unformatted')
    write(unit_no) 'Hello world!'
    rewind unit_no
    read(unit_no) q
    close(unit_no)
    print *, q
    if (q /= 'Hello world!') error stop
end program file_49
