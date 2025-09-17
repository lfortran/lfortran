! Test passing substring to subroutine
program string_72
    character(10):: str
    str = "123456789 "

    call ss(str(3:6))

    print *, str
    if(str /= "12abcd789 ") error stop

    contains 

    subroutine ss(str)
        character(*) :: str
        str = "abcd"
    end subroutine
end program string_72