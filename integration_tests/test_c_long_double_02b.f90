module test_c_long_double_02b
use iso_c_binding, only: c_int, ldk => c_long_double
implicit none
integer, parameter :: wp = ldk

interface
    integer(c_int) function c_long_double_size() bind(c, name="c_long_double_size")
    import :: c_int
    end function
end interface

end module
