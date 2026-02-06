! Test for https://github.com/lfortran/lfortran/issues/4647
! Scratch file with form='unformatted'
! Exact MRE from issue body
program file_49
    implicit none
    character :: q*12
    open(42, status='scratch', form='unformatted')
    write(42) 'Hello world!'
    rewind 42
    read(42) q
    close(42)
    print *, q
    if (q /= 'Hello world!') error stop
end program file_49
