program array_slice_01
implicit none

integer, target :: a(5, 6, 7, 19), sum_slice
integer, pointer :: b(:, :), c(:)
integer :: i, j, suma

b => a(:, 3, 4:6, 17)
a = 20
sum_slice = sum_array_slice(b)
print *, sum_slice
if( sum_slice /= 285 ) error stop

do i = lbound(a, 1), ubound(a, 1)
    do j = 4, 6
        if( a(i, 3, j, 17) /= 19 ) error stop
    end do
end do

suma = sum(a)
print *, suma
if( suma /= 79785 ) error stop

contains

function sum_array_slice(array) result(r)
integer, intent(inout) :: array(:, :)
integer :: i, j
integer :: r
r = 0
do i = lbound(array, 1), ubound(array, 1)
    do j = lbound(array, 2), ubound(array, 2)
      array(i, j) = 19
      r = r + array(i, j)
    end do
end do
end function

end program
