program arrays_reshape_15
implicit none
real, dimension(6) :: arr = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
real :: s(2, 3)
real, parameter :: eps = 1e-5

s = reshape(arr, shape(s))
end program
