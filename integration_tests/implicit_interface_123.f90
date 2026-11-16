! Procedures with an implicit interface that are defined in another file,
! called through an `external` statement, an `external` attribute and
! without declaration. WebAssembly links a call only to a definition with
! the same signature.
program implicit_interface_123
    implicit none
    external :: implicit_interface_123_solve
    real(8), external :: implicit_interface_123_dot
    real(8) :: a(2, 2), b(2), d
    integer :: info
    a = reshape([2d0, 1d0, 1d0, 3d0], [2, 2])
    b = [3d0, 4d0]
    call implicit_interface_123_solve(2, a, 2, b, info)
    print *, info, b
    if (info /= 0) error stop
    if (abs(b(1) - 1d0) > 1d-14 .or. abs(b(2) - 1d0) > 1d-14) error stop
    d = implicit_interface_123_dot(2, b, b)
    print *, d
    if (abs(d - 2d0) > 1d-14) error stop
    call implicit_interface_123_scale(2, b, 2d0)
    print *, b
    if (abs(b(1) - 2d0) > 1d-14 .or. abs(b(2) - 2d0) > 1d-14) error stop
end program
