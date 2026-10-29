program pass_array_by_data_17
    use pass_array_by_data_17_module
    implicit none
    type(accumulator), target :: storage
    class(accumulator), pointer :: object
    real :: x(12), matrix(4,4)
    integer :: i, j
    x = [(real(i), i=1,12)]
    do j = 1, 4
        do i = 1, 4
            matrix(i,j) = real(i + 4*(j-1))
        end do
    end do
    object => storage
    call object%add(x(::2), -3)
    print *, object%total
    if (object%total /= 36) error stop 1
    call object%add(x(::2), 5)
    if (object%total /= 36) error stop 2
    call object%add(x(12:2:-2), -3)
    if (object%total /= 42) error stop 3
    call object%add(x(1:0), -3)
    if (object%total /= 0) error stop 4
    call object%add_matrix(matrix(::2,::2))
    if (object%total /= 24) error stop 5
    call object%add_matrix(matrix(:,2:1))
    if (object%total /= 0) error stop 6
end program
