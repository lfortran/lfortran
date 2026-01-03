program bits1
    implicit none
    integer(8) :: arr2(3) = [1042890_8, 20_8, 30_8]

    print *, ibits(arr2, 2, 2)
end program
