program datastmt_01
    integer :: arr(3)
    integer :: arr2(3)
    integer :: arr3(3)
    integer :: arr4(3)
    data arr(1:3) /1, 2, 3/
    data arr2(1:2) /1, 2/
    data arr3(1:3) /3*1/
    data arr4(1:2) /2*1/

    if (any(arr /= [1,2,3])) error stop
    if (any(arr2(1:2) /= [1,2])) error stop
    if (any(arr3 /= [1,1,1])) error stop
    if (any(arr4(1:2) /= [1,1])) error stop
end program
