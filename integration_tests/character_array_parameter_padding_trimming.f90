!> These tests make sure that for constant(i.e. parameter) character string (or array)
!> * padding (when LHS length is higher)
!> * trimming (when LHS length is lower)
program character_array_parameter_padding_trimming
    character(len=1), parameter :: a_pad(4) = ["ab", "cd", "ef", "gh"]
    character(len=5), parameter :: b_pad(4) = ["ab", "cd", "ef", "gh"]

    character(len=1), parameter :: c_pad(4) = [["ab", "cd"], [["ef"], "gh"]]
    character(len=5), parameter :: d_pad(4) = [["ab", "cd"], [["ef"], "gh"]]

    if (len(a_pad) /= 1) error stop
    if (len(b_pad) /= 5) error stop
    if (len(c_pad) /= 1) error stop
    if (len(d_pad) /= 5) error stop
end program
