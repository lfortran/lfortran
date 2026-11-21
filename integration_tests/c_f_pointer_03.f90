program c_f_pointer_03
    ! An elemental intrinsic function reference as the SHAPE argument
    ! of c_f_pointer (#18295)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer, &
        c_int8_t, c_int64_t
    implicit none
    integer, target :: d(6)
    integer(c_int8_t), target :: pixels(24)
    integer, pointer :: p(:), q(:, :)
    integer(c_int8_t), pointer :: pixel(:, :, :)
    integer :: shp(1), shp2(2), n, nch, width, height, i
    integer(c_int64_t) :: shp8(1)
    type(c_ptr) :: cp

    d = [1, 2, 3, 4, 5, 6]
    cp = c_loc(d)
    shp = 3
    shp2 = [2, -3]
    shp8 = 4
    n = 5

    call c_f_pointer(cp, p, int(shp))
    if (size(p) /= 3) error stop
    if (any(p /= [1, 2, 3])) error stop

    call c_f_pointer(cp, p, int(shp, 8))
    if (size(p) /= 3) error stop
    if (p(3) /= 3) error stop

    call c_f_pointer(cp, p, int(shp8))
    if (size(p) /= 4) error stop
    if (p(4) /= 4) error stop

    call c_f_pointer(cp, p, int([n]))
    if (size(p) /= 5) error stop
    if (p(5) /= 5) error stop

    call c_f_pointer(cp, p, max(shp, n))
    if (size(p) /= 5) error stop
    if (any(p /= [1, 2, 3, 4, 5])) error stop

    call c_f_pointer(cp, q, abs(shp2))
    if (any(shape(q) /= [2, 3])) error stop
    if (q(2, 3) /= 6) error stop
    if (q(1, 2) /= 3) error stop

    ! gtk-fortran form
    nch = 2; width = 3; height = 4
    pixels = [(int(i, c_int8_t), i = 1, 24)]
    call c_f_pointer(c_loc(pixels), pixel, int([nch, width, height]))
    if (any(shape(pixel) /= [2, 3, 4])) error stop
    if (pixel(2, 3, 4) /= 24) error stop
    if (pixel(1, 2, 1) /= 3) error stop
    if (pixel(2, 1, 3) /= 14) error stop

    print *, "ok"
end program c_f_pointer_03
