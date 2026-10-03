program save_15
    implicit none
    if (next_value() /= 1) error stop
    if (next_value() /= 2) error stop
contains
    integer function next_value()
        integer, save :: value = 0
        value = value + 1
        next_value = value
    end function
end program save_15
