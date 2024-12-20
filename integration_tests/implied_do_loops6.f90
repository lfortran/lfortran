program implied_do_loops6

real, allocatable :: Q(:)
real :: R(5)
integer :: rank

allocate(Q(4))
Q = 5.0
Q([1, 3]) = -4.0

print *, Q
R = [abs(Q), 0.0_4]
print *, R

rank = maxval([0_4, trueloc(Q > 0)])
print *, rank

contains

function trueloc(x) result(loc)

logical, intent(in) :: x(:)
integer(4), allocatable :: loc(:)
integer :: i, j

allocate(loc(count(x)))

j = 1
do i = 1, size(x)
    if( x(i) ) then
        loc(j) = i
        j = j + 1
    end if
end do

end function trueloc

end program
