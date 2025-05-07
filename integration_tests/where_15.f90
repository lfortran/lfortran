program where_15
implicit none

real(4), allocatable :: x0(:)
real(4), allocatable :: xl(:)
real(4), allocatable :: xu(:)
real(4) :: rhobeg
integer :: i

rhobeg = 0.5

allocate(x0(5), xl(5), xu(5))

xl = 0.5
xu = 3.0

do i = lbound(x0, 1), ubound(x0, 1)
    x0 = i
end do

where (x0 <= xl + 0.5 * rhobeg)
    x0 = xl
elsewhere(x0 < xl + rhobeg)
    x0 = xl + rhobeg
end where

where (x0 >= xu - 0.5 * rhobeg)
    x0 = xu
elsewhere(x0 > xu - rhobeg)
    x0 = xu - rhobeg
end where

print *, x0
if( any(x0 /= [3.0, 3.0, 3.0, 3.0, 3.0]) ) error stop

end program
