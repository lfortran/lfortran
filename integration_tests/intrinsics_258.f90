program intrinsics_258
    real :: x(5) = [1, 2, 3, 4, 5]
    integer:: mean 
    mean = sum(x) / 5
    print*, sum(abs(x - mean)**2) / (5 - merge(1, 0, .true.))
end program