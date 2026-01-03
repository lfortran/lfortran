program functions_12
implicit none

contains

logical function decodebase(string) result(r)
    implicit none
    character(len=*), intent(in) :: string
    character(len=len(string)) :: string_local
    real, parameter :: x = real(huge(1))
end function decodebase

end program
