module declaration1

integer(4), dimension(1,1), parameter :: &
        arr = reshape([1, 1],shape(arr),order=[2,1])

end module declaration1
