program functions_52
    integer, allocatable :: b
    b = my()

    if (b /= 44) error stop

    contains
        function my() result(ret)
            integer, allocatable :: ret
            ret = 44
        end function
end program
