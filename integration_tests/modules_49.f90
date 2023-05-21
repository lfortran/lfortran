module modules_49_fpm_strings
implicit none

contains

pure function replace(string, charset, target_char) result(res)
    character(*), intent(in) :: string
    character(len=1), intent(in) :: charset(:), target_char
    character(len(string)) :: res
    integer :: n, m, one
    one = 1
    res = string
    do n = 1, len(string)
        ! TODO: To be fixed, after making any work for character types
        ! if (any(string(n:n) == charset)) then
        !     res(n:n) = target_char
        ! end if
        do m = lbound(charset, 1), ubound(charset, 1)
            if(string(n:n) == charset(m)) then
                res(n:n) = target_char(one:one)
                exit
            end if
        end do
    end do
end function replace

pure function to_fortran_name(string) result(res)
    character(*), intent(in) :: string
    character(len(string)) :: res
    character, parameter :: SPECIAL_CHARACTERS(*) = ['-']
    res = replace(string, SPECIAL_CHARACTERS, '_')
end function to_fortran_name

end module

program modules_49
use modules_49_fpm_strings
implicit none

character(len=40) :: name = "--gnu-gfortran-11"
print *, name, to_fortran_name(name)
if( to_fortran_name(name) /= "__gnu_gfortran_11" ) error stop

end program
