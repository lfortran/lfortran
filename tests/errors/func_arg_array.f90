program test_array_rank
    implicit none
    integer :: i, j
    integer, dimension(3, 3) :: matrix = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
    i=print_flat_array(matrix)
    print * , i
end program test_array_rank

integer function print_flat_array(arr)
    integer, intent(in), dimension(12,3,1) :: arr
    print_flat_array = arr(1,1)
end function 

