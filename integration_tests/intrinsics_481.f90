program intrinsics_481
    ! Test co_broadcast intrinsic subroutine (no-op in single-image mode).
    ! With a single image, source_image==1 and co_broadcast is a no-op.
    implicit none
    integer :: i
    real :: r
    integer :: stat_val
    character(len=32) :: err_msg

    i = 42
    call co_broadcast(i, 1)
    if (i /= 42) error stop

    call co_broadcast(i, source_image=1)
    if (i /= 42) error stop

    call co_broadcast(i, 1, stat_val)
    if (i /= 42) error stop

    call co_broadcast(i, 1, stat_val, err_msg)
    if (i /= 42) error stop

    r = 3.5
    call co_broadcast(r, 1)
    if (abs(r - 3.5) > 1e-6) error stop
end program intrinsics_481