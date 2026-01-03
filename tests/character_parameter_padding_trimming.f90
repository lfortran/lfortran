!> These tests make sure that for constant(i.e. parameter) character string (or array)
!> * padding (when LHS length is higher)
!> * trimming (when LHS length is lower)
program character_parameter_padding_trimming
    !> initializate variables to test correct padding of ' ' to the end of character string
    character(len=8), parameter :: x_pad = "apple" !> char length on right is 5
    character(len=10), parameter :: y_pad = "Ball" !> char length on right is 4
    character(len=30), parameter :: z_pad = x_pad // y_pad // x_pad !> char length on right is 26
    character(len=4), parameter :: p_pad(3) = "2" !> char length on right is 1

    print *, "|" // x_pad // "|"
    print *, y_pad
    print *, p_pad

    !> initializate variables to test correct trimming of ' ' to the end of character string
    character(len=2), parameter :: x_trim = "apple" !> char length on right is 5
    character(len=3), parameter :: y_trim = "Ball" !> char length on right is 4
    character(len=5), parameter :: z_trim = x_trim // y_trim // x_trim !> char length on right is 26
    character(len=1), parameter :: p_trim(3) = "25" !> char length on right is 2

    print *, x_trim
    print *, y_trim
    print *, z_trim
    print *, p_trim

end program
