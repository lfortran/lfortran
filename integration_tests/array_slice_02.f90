program array_slice_02
implicit none

integer(1) :: decoder_txt(10) = (/97, 98, 99, 65, 66, 100, 67, 69, 70, 71/)
integer :: start, end
character(:), allocatable :: txt

start = 3
end  = 8
if( lbound(decoder_txt(start:end), 1) /= 1 ) error stop
if( ubound(decoder_txt(start:end), 1) /= end - start + 1 ) error stop
txt = c2s(decoder_txt(start:end))
print *, txt
if( txt /= 'cABdCE' ) error stop

contains

function c2s(x) result(y)

integer(1), intent(in) :: x(:)
character(:), allocatable :: y
integer :: i
print *, lbound(x, 1), ubound(x, 1), size(x)
if( lbound(x, 1) /= 1 ) error stop
if( ubound(x, 1) /= size(x) ) error stop
allocate(character(size(x)) :: y)
do i = 1, size(x)
    y(i:i) = char(int(x(i), 4))
end do

end function

end program
