program array_passing
    integer, dimension(3,2) :: myArray = reshape([1,2,3,4,5,6],[3,2])
    integer :: i
    i = temp(myArray)
end program array_passing

integer Function temp(arr)
    integer, dimension(2,2,2) :: arr
    temp = arr(1,2,2)
end function