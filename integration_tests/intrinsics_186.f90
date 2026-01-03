program intrinsic_186
    implicit none
    character(7) :: string
    print*, verify(maybe(string), "foo")
    if (verify(maybe(string), "foo") /= 3) error stop
    contains

    function maybe(string) result(maybe_string)
        character(7) :: string
        character(len=len(string)) :: maybe_string
        maybe_string = "fortran"
    end function maybe

end program
