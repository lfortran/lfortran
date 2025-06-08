module where_13_module
implicit none 
private
public :: is_pos
contains
    pure elemental function is_pos(x) result(y)
        implicit none
        real, intent(in) :: x
        logical :: y
        y = (x > 0)
    end function is_pos
end module where_13_module

program where_13
use, non_intrinsic :: where_13_module
real :: V(2) = [-1,2]
logical :: W(2) 
where (is_pos(V))
        V = 12.91
elsewhere
        V = -12.91
end where
print *, V
print *, sum(V)
if ( abs(sum(V)) > 1e-8) error stop
end program where_13
