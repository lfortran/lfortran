program cycle_and_exit5
implicit none

integer :: i, j
integer(8) :: s
s = 0
do i = 1, 10
    if( 3*(i/3) == i ) exit
    do j = 1, 20
        s = s + i + j
    end do
    if( 2*(i/2) == i ) cycle
    s = 2*s
end do

print *, s
if( s/= 710 ) error stop

end program cycle_and_exit5
