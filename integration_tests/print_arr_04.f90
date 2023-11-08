program expr2
implicit none

integer :: _arr(1) = [89]
if (_arr(1) /= 89) error stop
print *, _arr

end program
