program string_56
    character(len=:), pointer :: temp
    character(len=:), allocatable :: str
    temp => cast_string()
    str = temp
    if (str /= "Hello") error stop
contains 
    function cast_string() result(ptr)
        character(:), pointer :: ptr
        character(len=5), save, target ::tmp = "Hello"
        ptr => tmp
    end function cast_string
end program