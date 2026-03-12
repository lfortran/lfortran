program write_and_read_temp
    character(len=20) :: buf
    integer(8) :: stat, major, temp = 1
    character(len=20) :: filename

    filename = "tempfile.txt"

    open(unit=10, file=filename, status='replace', access="stream", form="unformatted")

    write(10) "HELLOWORLD"
    close(10)
    open(unit=10, file=filename, status='old', access="stream", form="unformatted")
    major = 2   ! Try changing this to 1

    read(10, iostat=stat) buf(1:merge(4, 2, major > 1))

    print *, "Status code:", stat
    print *, "From file:", trim(buf)

    close(10, status="delete")
end program write_and_read_temp