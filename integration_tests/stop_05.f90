program stop_05
    ! The stop code is an array element: its value is the code.
    implicit none
    integer :: codes(3)
    codes = [3, 0, 5]
    stop codes(2)
end program
