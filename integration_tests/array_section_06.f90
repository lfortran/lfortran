program array_section_06
    integer :: arr(2,3)
    arr(1,1) = 1
    arr(2,1) = 2
    arr(1,2) = 3
    arr(2,2) = 4
    arr(1,3) = 5
    arr(2,3) = 6
    print * , arr(1,:)
    print * , arr(2,:)
    if (.not. all(arr(1,:) == [1, 3, 5])) error stop
    if (.not. all(arr(2,:) == [2, 4, 6])) error stop
end program