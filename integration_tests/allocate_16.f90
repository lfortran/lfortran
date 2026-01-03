program allocate_16
implicit none

integer :: i, s

s = 0
call prg(10, 10, 10, s)

print *, s
if( s /= 200000000 ) error stop

contains

subroutine prg(d1, d2, d3, s)
integer, intent(in) :: d1, d2, d3
integer, intent(out) :: s
integer, allocatable :: d(:, :, :)
s = 0
do i = 1, 200000
    allocate(d(d1, d2, d3))
    s = s + size(d)
    deallocate(d)
end do
end subroutine

end program
