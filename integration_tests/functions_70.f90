! The result of a function is not returned into the variable it is assigned
! to when an internal procedure passed to the function reads that variable by
! host association (F2018 10.2.1.3, #16191).
module functions_70_m
implicit none
contains

function f(p) result(r)
    interface
        function p() result(q)
            real :: q
        end function
    end interface
    real :: r(3)
    r(1) = 100
    r(2) = p() + 10
    r(3) = p() + 20
end function

end module

program functions_70
use functions_70_m
implicit none
real :: v(3), y(3)

v = [1.0, 2.0, 3.0]
if (abs(v(1) - 1.0) > 1e-6) error stop 1
v = f(first_of_v)
print *, v
if (any(abs(v - [100.0, 11.0, 21.0]) > 1e-6)) error stop 2

v = [1.0, 2.0, 3.0]
if (abs(v(1) - 1.0) > 1e-6) error stop 3
y = f(first_of_v)
print *, y
if (any(abs(y - [100.0, 11.0, 21.0]) > 1e-6)) error stop 4

contains

function first_of_v() result(q)
    real :: q
    q = v(1)
end function

end program
