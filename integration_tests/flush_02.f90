program flush_02
    implicit none
    open(10, file="file_01_data.txt")
    open(11, file="file_02_data.txt")
    call FLUSH()
    close(10)
    close(11)
end program flush_02
