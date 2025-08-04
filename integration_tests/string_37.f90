program string_37
    character(5) :: string
    character(:), pointer :: string2 
    print*, max(maybe(string), 'works')
    if (max(maybe(string), 'works') /= 'works') error stop 
    string2 => cast_to_string()
    string = string2
    if (string /= "Hello") error stop
    contains

    function maybe(string) result(maybe_string)
        character(5) :: string
        character(len=len(string)) :: maybe_string
    end function maybe
    function cast_to_string() result(str)
        character(len=:), pointer :: str
        character(len=5), target :: str2
        str2 = "Hello"
        str => str2
    end function cast_to_string
end
