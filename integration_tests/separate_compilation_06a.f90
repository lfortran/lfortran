module separate_compilation_06a_module

contains


function get_midpoints(R, V) result(Vmid)
real, intent(in) :: R(:), V(:)
real :: Vmid(size(R)-1)
integer :: i
print *, sum(R), sum(V)
Vmid = 0.5 * (V(1:size(R)-1) + V(2:size(R)))
if ( abs(sum(R) - 4.96500000e+02 ) > 1e-8 ) error stop
if ( abs(sum(V) - 5.01404822e-01 ) > 1e-8 ) error stop
end function

end module


