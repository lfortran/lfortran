program array_indices_array_item_assignment_3
    implicit none
    type :: line
        integer :: first
    end type

    integer :: arr(5) = [1, 2, 3, 4, 5]
    integer :: shift(5)
    type(line) :: l_arr(5)

    integer :: i
    do i = 1, 5
        l_arr(i)%first = i*10
    end do

    shift(:) = l_arr(arr)%first - 1

    if (any([9, 19, 29, 39, 49] /= shift)) error stop
end program array_indices_array_item_assignment_3
