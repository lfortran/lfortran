program flush2
    implicit none
    call flush()
    open(10, file="file_01_data.txt")
    call flush(10)
end program flush2
