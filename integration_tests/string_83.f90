module string_83_mod
    implicit none
    public
    character(len=*), parameter :: char_arr_global(4, 1) = reshape([ character(len=30) :: &
        "YES",&
        "\d                    ",&
        "5                             ",&
        "1 "], [4,1])

    contains

    subroutine sub(str)
        character(*), intent(out) :: str

        character(len(char_arr_global)) :: buffer
        if(len(buffer) /= 30) error stop

        if(len(char_arr_global(1, 1)) /= 30) error stop
        if(trim(char_arr_global(1, 1)) /= "YES") error stop
        if(trim(char_arr_global(2, 1)) /= "\d") error stop
        if(trim(char_arr_global(3, 1)) /= "5") error stop
        if(trim(char_arr_global(4, 1)) /= "1") error stop

        if(len(str) /= 30) error stop
        str = char_arr_global(3, 1)
    end subroutine

end module


program string_83
    use string_83_mod
    implicit none
    character(len=30) :: str
    character(:), allocatable :: write_buffer
    call sub(str)
    
    if(str /= char_arr_global(3, 1)) error stop

    allocate(character(len=len(char_arr_global)* size(char_arr_global)) :: write_buffer)
    write(write_buffer, "(4A)") char_arr_global
    print *, write_buffer
    if(write_buffer /= "YES                           &
                        \d                            &
                        5                             &
                        1                             ") error stop

end program
