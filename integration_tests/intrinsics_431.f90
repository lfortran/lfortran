program intrinsics_431
    implicit none
    character :: ch
    integer :: j

    ch = char(0)
    print *, ch
    print *, ichar(ch)
    if (ichar(ch) /= 0) error stop

    ch = achar(0)
    j = iachar(ch)
    print *, j
    if (j /= 0) error stop

    print *, is_control(char(0))
    if (.not. is_control(char(0))) error stop
    print *, is_control(char(65))
    if (is_control(char(65))) error stop
    print *, is_control(char(127))
    if (.not. is_control(char(127))) error stop

contains

    logical function is_control(c)
        character(len=1), intent(in) :: c
        integer :: ic
        ic = iachar(c)
        is_control = ic < int(z'20') .or. ic == int(z'7F')
    end function

end program intrinsics_431
