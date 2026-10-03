program transfer_18
    implicit none
    character(1) :: value = "a"
    integer, allocatable :: val(:)

    val = transfer(value, val, 1)
    if (size(val) /= 1) error stop
end program transfer_18