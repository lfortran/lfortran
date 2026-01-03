program merge_bits_run
    integer :: a, c 
    integer(kind=8) :: b
    a = 8
    b = 12_8
    c = 2
    print *, merge_bits(a, b, c)
end program
