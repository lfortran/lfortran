program read_124
    ! List-directed internal read of real(10): scalar and a length-1 array.
    ! Kind 10 exists only where the C long double kind is 10.
    use iso_c_binding, only: c_long_double
    implicit none
    integer, parameter :: kp = c_long_double
    real(kp) :: x, y(1)
    character(len=4) :: s
    integer :: ios

    if (kp /= 10) stop 0

    s = "1.25"
    read(s, *, iostat=ios) x
    if (ios /= 0) error stop
    if (x /= 1.25_kp) error stop

    ios = 1
    read(s, *, iostat=ios) y
    if (ios /= 0) error stop
    if (y(1) /= 1.25_kp) error stop
end program read_124
