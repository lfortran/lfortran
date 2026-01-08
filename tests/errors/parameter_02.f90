program parameter_02
    integer :: arr(3) = [5, 10, 15]
    integer, parameter :: result = minloc(arr, 1, [.false., .false., .false.])
    print *, result
end program
