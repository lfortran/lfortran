program class_21
    type :: val_type
        integer :: origin = 3
    end type
    integer :: stat
    class(val_type), allocatable :: val
    allocate(val)
    stat = merge(val%origin, stat, .true.)

    if (stat /= 3) error stop
end program
