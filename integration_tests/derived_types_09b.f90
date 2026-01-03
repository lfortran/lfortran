module derived_types_09b
use derived_types_09c, only: data_t

contains

integer function f(d)
type(data_t), intent(inout) :: d
real :: A(size(d%A))
end function

end module
