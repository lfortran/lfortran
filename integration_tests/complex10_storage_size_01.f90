program complex10_storage_size_01
    implicit none
    integer, parameter :: xdp = selected_real_kind(18)
    complex(xdp) :: z
    complex(10) :: y
    integer :: n

    z = (1.0_xdp, 2.0_xdp)
    print *, xdp, real(z), aimag(z)
    if (xdp /= 10) error stop "xdp"
    if (real(z) /= 1.0_xdp) error stop "real"
    if (aimag(z) /= 2.0_xdp) error stop "aimag"

    y = (1.0_10, 2.0_10)
    n = storage_size(y)
    print *, n
    if (n /= 256) error stop "storage_size"
    if (storage_size(z) /= 256) error stop "storage_size(z)"
    if (storage_size(real(y)) /= storage_size(1.0_10)) error stop "storage_size(real)"
    if (kind(y) /= 10) error stop "kind"
end program
