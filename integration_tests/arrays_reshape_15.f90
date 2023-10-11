program arrays_reshape_15
implicit none
real, dimension(6) :: arr = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
real :: s(2, 3)
integer :: i, j

s = reshape(arr, shape(s))

do i = 1, 2
    do j = 1, 3
        write(*, '(F5.1)', advance='no') s(i, j)
    end do
    write(*, *)
end do

end program
