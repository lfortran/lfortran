module format_18_mod
    implicit none
    contains 
    character(100) function logical_to_string(value, format) 
        logical :: value
        character(len=*) :: format
        ! character(len=100), allocatable :: string

        character(len=100) :: buffer
        integer :: stat

        ! If format isn't equal to `l`, it should raise a runtime error
        ! but instead it would just set stat to not 0 (which indicates an error)
        write(buffer, "(" // format // ")", iostat=stat) value ! Note : if iostat isn't passed a runtime error would be raised.
        if (stat == 0) then
            logical_to_string = trim(buffer)
        else
            logical_to_string = "[*]"
        end if
    end function logical_to_string
end module format_18_mod

program name
    use format_18_mod
    implicit none
    logical :: logi 
    character(100) :: str
    logi = .true. 
    str = logical_to_string(logi, "(I5)") 
    print *, str
    if(str /= "[*]") error stop
end program name

