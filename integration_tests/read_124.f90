program read_124
    ! List-directed internal read of real(10): scalar and a length-1 array.
    ! LFortran accepts an explicit real(10) on every host and lowers it to
    ! the kind-10 reader, even where c_long_double is not 10. GFortran
    ! rejects a kind it does not have, so that path never spells kind 10.
    use iso_c_binding, only: c_long_double
    implicit none
#ifdef __LFORTRAN__
    integer, parameter :: kp = 10
#else
    integer, parameter :: kp = merge(10, 8, c_long_double == 10)
#endif
    real(kp) :: x, y(1)
    character(len=4) :: s
    integer :: ios

    if (c_long_double < 0) stop 0
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
