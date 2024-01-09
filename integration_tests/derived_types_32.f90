module testdrive_derived_types_32
    implicit none
    private

    public :: real_dp_to_string

contains

pure function real_dp_to_string(val) result(string)
    real(8), intent(in) :: val
    character(len=:), allocatable :: string
    integer, parameter :: buffer_len = 128
    character(len=buffer_len) :: buffer

    write(buffer, '(g0)') val
    string = trim(buffer)

end function real_dp_to_string

end module testdrive_derived_types_32


program main
use testdrive_derived_types_32
implicit none

real(8) :: value
value = 10.0

print *, real_dp_to_string(value)
if( real_dp_to_string(value) /= "10.000000000000000" ) error stop

end program
