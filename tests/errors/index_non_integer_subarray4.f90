program index_non_integer_subarray4
    integer :: x(7)
    x = [1, 2, 3, 4, 5, 6, 7]
    print *, x(1:7:1.2)
end program
