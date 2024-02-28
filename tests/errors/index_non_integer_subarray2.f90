program index_non_integer_subarray2
    integer :: x(7)
    x = [1, 2, 3, 4, 5, 6, 7]
    print *, x(1:2.3)
end program
