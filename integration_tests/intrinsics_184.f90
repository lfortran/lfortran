program intrinsics_184
    implicit none
    integer, parameter :: ascii = selected_char_kind ("ascii")
    integer, parameter :: ascii_case = selected_char_kind ("ASciI")
    integer, parameter :: default  = selected_char_kind ("default")
    integer, parameter :: ucs4  = selected_char_kind ("ISO_10646")
    integer, parameter :: other = selected_char_kind ("other")
    character(5) :: ascii_str = "ascii"
    character(5) :: ascii_case_str = "ASciI"
    character(7) :: default_str = "default"
    character(9) :: ucs4_str = "ISO_10646"
    character(5) :: other_str = "other"

    print*, selected_char_kind(ascii_str)
    if (selected_char_kind(ascii_str) /= 1) error stop
    print*, selected_char_kind(ascii_case_str)
    if (selected_char_kind(ascii_case_str) /= 1) error stop
    print*, selected_char_kind(default_str)
    if (selected_char_kind(default_str) /= 1) error stop
    print*, selected_char_kind(ucs4_str)
    if (selected_char_kind(ucs4_str) /= 4) error stop
    print*, selected_char_kind(other_str)
    if (selected_char_kind(other_str) /= -1) error stop
    print*, ascii
    if (ascii /= 1) error stop
    print*, ascii_case
    if (ascii_case /= 1) error stop
    print*, default
    if (default /= 1) error stop
    print*, ucs4
    if (ucs4 /= 4) error stop
    print*, other
    if (other /= -1) error stop
    
end program
