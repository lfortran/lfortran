! Test Optional array of strings
program string_61
    character(10):: ss
    character(10) :: arr(2)
    
    ss = ff()
    print *, ss
    if(ss /= "Hello") error stop

    arr(1) = "world"
    ss = ff(arr)
    print *, ss
    if(ss /= "world") error stop
    
    contains

    function ff(string) result(ret)
        character(10), optional, dimension(0:) :: string
        character(10) :: ret
        if(.not.present(string)) then
            ret = "Hello"
        else 
            ret = string(0)
        end if
    end function
end program