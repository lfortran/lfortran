! Test scratch file with unformatted I/O (issue #4647)
program file_49
    implicit none
    character :: q*12

    open(42, status='scratch', form='unformatted')
    write(42) 'Hello world!'
    rewind 42
    read(42) q
    close(42)

    if (q /= 'Hello world!') error stop
    print *, q
end program file_49
