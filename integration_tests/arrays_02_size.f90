program arrays_01
implicit none
integer :: i, a(3), b(4)
integer(8) :: size_a8
integer(8) :: size_b8
integer(4) :: size_a4
integer(4) :: size_b4
size_a4 = size(a, kind=4)
size_b4 = size(b, dim=1, kind=4)
size_a8 = size(a, kind=8)
size_b8 = size(b, dim=1, kind=8)

if( size_a4 /= 3 ) error stop
if( size_b4 /= 4 ) error stop

if( size_a8 /= 3_8 ) error stop
if( size_b8 /= 4_8 ) error stop

end
