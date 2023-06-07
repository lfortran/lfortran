program flush_01
    integer :: ret
    open(10, file="foo")
    flush(10)
end program flush_01
