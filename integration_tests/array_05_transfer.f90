program array_05_transfer
    implicit none
    real :: value(5) = [1.1, 1.2, 1.3, 1.4, 1.5]
    integer, allocatable :: val(:)
    val = transfer(value, val, 5)
    print * , val
    if (all(val /= [1066192077, 1067030938, 1067869798, 1068708659, 1069547520])) error stop
end program
