module character_02_mod
    implicit none
contains
    pure integer function get_length(i, fmt) result(o)
        integer, intent(in) :: i
        character(*), intent(in) :: fmt
        integer, parameter :: MAX_STR = 100
        character(MAX_STR) :: s
        write(s, fmt) i
        o = len_trim(s)
    end function get_length
    pure function get_string(i) result(o)
        integer, intent(in) :: i
        character(4), parameter :: fmt = "(i0)"
        character(len=get_length(i, fmt)) :: o
        write(o, fmt) i
    end function get_string
end module character_02_mod

program character_02
    use character_02_mod
    implicit none
    if (get_string(3252) /= "3252") error stop
end program character_02
