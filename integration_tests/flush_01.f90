program flush_01
    integer :: ret
    open(10, file="file_01_data.txt")
    flush(10)
end program flush_01
