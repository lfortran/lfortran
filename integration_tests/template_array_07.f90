module template_array_07_m
    implicit none
    template array_intrinsics {t}
        deferred type :: t
    contains
        function evaluate(a, b, mask) result(r)
            type(t), intent(in) :: a(4), b(4)
            logical, intent(in) :: mask(4)
            type(t) :: r(4, 5)

            r(:, 1) = merge(a, b, mask)
            r(:, 2) = merge(a, b, mask(2))
            r(:, 3) = cshift(a, 1)
            r(:, 4) = pack(a, mask, b)
            r(:, 5) = pack(reshape(a, [2, 2]), .true.)
        end function
    end template
end module

program template_array_07
    use template_array_07_m
    implicit none
    ! Experimental template syntax is not supported by GFortran.
    instantiate array_intrinsics {integer}, only: evaluate_i4 => evaluate
    instantiate array_intrinsics {integer(8)}, only: evaluate_i8 => evaluate
    instantiate array_intrinsics {real}, only: evaluate_r4 => evaluate
    instantiate array_intrinsics {real(8)}, only: evaluate_r8 => evaluate
    instantiate array_intrinsics {logical}, only: evaluate_l => evaluate
    instantiate array_intrinsics {complex(8)}, only: evaluate_c8 => evaluate

    integer :: i4(4, 5)
    integer(8) :: i8(4, 5)
    real :: r4(4, 5)
    real(8) :: r8(4, 5)
    logical :: l(4, 5), mask(4)
    complex(8) :: c8(4, 5)
    integer(8), parameter :: ix = 5000000000_8, iy = -3000000000_8
    real, parameter :: sx = 1.25, sy = -3.5
    real(8), parameter :: dx = 2.5_8, dy = -7.125_8
    logical, parameter :: lx = .true., ly = .false.
    complex(8), parameter :: cx = (1.0_8, 2.0_8), cy = (3.0_8, -4.0_8)

    mask = [.true., .false., .true., .false.]

    i4 = evaluate_i4([7, -3, -3, 7], [-3, 7, 7, -3], mask)
    if (any(i4 /= reshape([7, 7, -3, -3, &
                          -3, 7, 7, -3, &
                          -3, -3, 7, 7, &
                          7, -3, 7, -3, &
                          7, -3, -3, 7], [4, 5]))) error stop

    i8 = evaluate_i8([ix, iy, iy, ix], [iy, ix, ix, iy], mask)
    if (any(i8 /= reshape([ix, ix, iy, iy, &
                          iy, ix, ix, iy, &
                          iy, iy, ix, ix, &
                          ix, iy, ix, iy, &
                          ix, iy, iy, ix], [4, 5]))) error stop

    r4 = evaluate_r4([sx, sy, sy, sx], [sy, sx, sx, sy], mask)
    if (any(abs(r4 - reshape([sx, sx, sy, sy, &
                             sy, sx, sx, sy, &
                             sy, sy, sx, sx, &
                             sx, sy, sx, sy, &
                             sx, sy, sy, sx], [4, 5])) > 1e-6)) error stop

    r8 = evaluate_r8([dx, dy, dy, dx], [dy, dx, dx, dy], mask)
    if (any(abs(r8 - reshape([dx, dx, dy, dy, &
                             dy, dx, dx, dy, &
                             dy, dy, dx, dx, &
                             dx, dy, dx, dy, &
                             dx, dy, dy, dx], [4, 5])) > 1e-12_8)) error stop

    l = evaluate_l([lx, ly, ly, lx], [ly, lx, lx, ly], mask)
    if (any(l .neqv. reshape([lx, lx, ly, ly, &
                             ly, lx, lx, ly, &
                             ly, ly, lx, lx, &
                             lx, ly, lx, ly, &
                             lx, ly, ly, lx], [4, 5]))) error stop

    c8 = evaluate_c8([cx, cy, cy, cx], [cy, cx, cx, cy], mask)
    if (any(abs(c8 - reshape([cx, cx, cy, cy, &
                             cy, cx, cx, cy, &
                             cy, cy, cx, cx, &
                             cx, cy, cx, cy, &
                             cx, cy, cy, cx], [4, 5])) > 1e-12_8)) error stop
end program
