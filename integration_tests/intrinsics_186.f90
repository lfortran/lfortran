program intrinsic_186
    
    character(7) :: string
    print*, verify(maybe(string), "foo")
    contains

    function maybe(string) result(maybe_string)
        character(7) :: string
        character(len=len(string)) :: maybe_string
        maybe_string = "fortran"
    end function maybe

end program
