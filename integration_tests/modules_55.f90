module stdlib_kinds_modules_55
    use iso_fortran_env, only: int64
    implicit none
    private
    public :: int64

end module stdlib_kinds_modules_55

module stdlib_hashmaps_modules_55
    use stdlib_kinds_modules_55, only: int64

    implicit none
    private

    integer, parameter, public :: int_calls = int64

end module stdlib_hashmaps_modules_55

program modules_55
use stdlib_hashmaps_modules_55
implicit none

print *, int_calls
if( int_calls /= 8 ) error stop

end program
