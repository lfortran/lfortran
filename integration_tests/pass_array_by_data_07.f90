module pass_array_by_data_07_module
implicit none

contains

function gdef(x)
    real, intent(in) :: x(:)
    real :: gdef(size(x))
    gdef = x + 1.0
end function

end module

program pass_array_by_data_07
use pass_array_by_data_07_module, only: gdef
implicit none

real :: array(10)
array = 2.0
print *, f(gdef1, array)
if( f(gdef1, array) /= 20.0 ) error stop

print *, f(gdef, array)
if( f(gdef, array) /= 40.0 ) error stop

contains

function f(g, array) result(s)
real, intent(in) :: array(:)
real :: s
interface
    function g(x)
        real, intent(in) :: x(:)
        real :: g(size(x))
    end function
end interface
real :: array1(size(array))
array1 = array + 1.0
array1 = g(array1)
s = sum(array1)
end function

function gdef1(x)
    real, intent(in) :: x(:)
    real :: gdef1(size(x))
    gdef1 = x - 1.0
end function

end program
