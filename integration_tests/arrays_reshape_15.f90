program arrays_reshape_15
implicit none
real, dimension(6) :: arr = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
real :: s(2, 3)
integer :: i, j
real, parameter :: eps = 1.0

s = reshape(arr, shape(s))

if (abs(s(1, 1) - 1.0) > eps) error stop
if (abs(s(2, 1) - 2.0) > eps) error stop
if (abs(s(1, 2) - 3.0) > eps) error stop
if (abs(s(2, 2) - 4.0) > eps) error stop
if (abs(s(1, 3) - 5.0) > eps) error stop
if (abs(s(2, 3) - 6.0) > eps) error stop

do i = 1, 2
    do j = 1, 3
        write(*, '(F5.1)', advance='no') s(i, j)
    end do
    write(*, *)
end do

end program
