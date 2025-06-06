program character_09
    implicit none
    integer, parameter :: SCK = selected_char_kind("ascii")
    character(kind=SCK) :: one_len
    one_len = "hello"
    if (one_len /= "h") error stop
end program
