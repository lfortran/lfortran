program data_24
    implicit none
    integer :: values(2)
    data values([1, 2]) /3, 4/
    if (any(values /= [3, 4])) error stop
    print *, "test passed"
end program
