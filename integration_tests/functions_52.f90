program functions_52
    integer, allocatable :: b

    if (allocated(b) /= .false.) error stop
    b = my()
    if (allocated(b) /= .true.) error stop

    if (b /= 44) error stop

    contains
        function my() result(ret)
            integer, allocatable :: ret
            ret = 44
        end function
end program
