program string_63
    type :: string_type
        character(len=:), allocatable :: raw
    end type string_type
    
    type(string_type) :: string

    string%raw = "Hello"

    string = ff(string)

    print *, string%raw
    if(string%raw /= "Hello!!") error stop

    contains 

    function ff(s) result(string) 
       type(string_type),intent(in) :: s
       type(string_type) :: string
       print *, s%raw
       if(s%raw /= "Hello") error stop
       ! This should work with no issue (but as to `subroutine_from_function` is using LHS to be the holder of the return, allocating return `string` means allcoating argument `s`)
    !    allocate(character(len(s%raw)+2) :: string%raw) 
       string%raw = s%raw //"!!"
    end function 
end program