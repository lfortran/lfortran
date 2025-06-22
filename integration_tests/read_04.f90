program read_04
    character(len=20) :: buf
    integer :: stat, major

    open(unit=10, file='read_04_data.txt', status='old', access="stream", form="unformatted")
    major = 2

    read(10, iostat=stat) buf(1:merge(4, 2, major > 1))

    close(10)
end program read_04
