program intrinsics_107
    integer :: x(5) = (/ 1, 2, 3, 4 ,5 /)
    logical :: mask(5) = (/ .TRUE., .FALSE., .TRUE., .FALSE., .TRUE. /)
    if ( .not. sum(array=x,mask=mask) == 9) error stop
end
