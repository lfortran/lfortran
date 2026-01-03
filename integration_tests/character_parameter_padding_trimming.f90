!> These tests make sure that for constant(i.e. parameter) character string (or array)
!> * padding (when LHS length is higher)
!> * trimming (when LHS length is lower)
program character_parameter_padding_trimming
    !> initializate variables to test correct padding of ' ' to the end of character string
    character(len=8), parameter :: x_pad = "apple" !> char length on right is 5
    character(len=10), parameter :: y_pad = "Ball" !> char length on right is 4
    character(len=30), parameter :: z_pad = x_pad // y_pad // x_pad !> char length on right is 26
    character(len=32), parameter :: input = "reversed" !> char length on right is 8
    character(len=4), parameter :: p_pad(3) = "2" !> char length on right is 1

    !> initializate variables to test correct trimming of ' ' to the end of character string
    character(len=2), parameter :: x_trim = "apple" !> char length on right is 5
    character(len=3), parameter :: y_trim = "Ball" !> char length on right is 4
    character(len=5), parameter :: z_trim = x_trim // y_trim // x_trim !> char length on right is 26
    character(len=2), parameter :: input_trim = "reversed" !> char length on right is 8
    character(len=1), parameter :: p_trim(3) = "25" !> char length on right is 2

    !> tests to assert padding is done correctly
    if (len(x_pad) /= 8) error stop
    if (len(y_pad) /= 10) error stop
    if (len(z_pad) /= 30) error stop
    if (len(p_pad(1)) /= 4) error stop
    if (len(p_pad(2)) /= 4) error stop
    if (len(p_pad(3)) /= 4) error stop

    !> tests to assert trimming is done correctly
    if (len(x_trim) /= 2) error stop
    if (len(y_trim) /= 3) error stop
    if (len(z_trim) /= 5) error stop
    if (len(p_trim(1)) /= 1) error stop
    if (len(p_trim(2)) /= 1) error stop
    if (len(p_trim(3)) /= 1) error stop
end program
