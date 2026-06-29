program character_array_block_padding
    implicit none
    integer :: i

    block
        character(20), dimension(4) :: methods = [ &
             "zero      ", "same      ", "valid     ", "full      " ]

        if (len(methods) /= 20) error stop "wrong elem len"
        if (size(methods) /= 4) error stop "wrong size"

        if (len_trim(methods(1)) /= 4) error stop "elem 1 len_trim"
        if (len_trim(methods(2)) /= 4) error stop "elem 2 len_trim"
        if (len_trim(methods(3)) /= 5) error stop "elem 3 len_trim"
        if (len_trim(methods(4)) /= 4) error stop "elem 4 len_trim"

        if (trim(methods(1)) /= "zero")  error stop "elem 1 value"
        if (trim(methods(2)) /= "same")  error stop "elem 2 value"
        if (trim(methods(3)) /= "valid") error stop "elem 3 value"
        if (trim(methods(4)) /= "full")  error stop "elem 4 value"

        do i = 1, 4
            print *, i, len_trim(methods(i)), "['"//trim(methods(i))//"']"
        end do
    end block
end program
