program intrinsics_93
    integer :: i 
    integer(8) :: j
    real :: x 
    real(8) :: y
    if (digits(i) /= 31) error stop
    if (digits(j) /= 63) error stop
    if (digits(x) /= 24) error stop
    if (digits(y) /= 53) error stop
end