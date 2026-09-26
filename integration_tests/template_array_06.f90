module template_array_06_m
    implicit none
    template array_results {t}
        deferred type :: t
    contains
        function evaluate(x, y) result(r)
            type(t), intent(in) :: x, y
            type(t) :: r(2, 2), z(1, 2)

            z(1, 1) = x
            z(1, 2) = y
            r(:, 1) = [x, y]
            r(:, 2) = reshape(z, [2])
        end function
    end template
end module

program template_array_06
    use template_array_06_m
    implicit none
    ! Experimental template syntax is not supported by GFortran.
    instantiate array_results {integer}, only: evaluate_i4 => evaluate
    instantiate array_results {integer(8)}, only: evaluate_i8 => evaluate
    instantiate array_results {real}, only: evaluate_r4 => evaluate
    instantiate array_results {real(8)}, only: evaluate_r8 => evaluate
    instantiate array_results {logical}, only: evaluate_l => evaluate
    instantiate array_results {complex(8)}, only: evaluate_c8 => evaluate

    integer :: i4(2, 2)
    integer(8) :: i8(2, 2)
    real :: r4(2, 2)
    real(8) :: r8(2, 2)
    logical :: l(2, 2)
    complex(8) :: c8(2, 2)
    integer(8), parameter :: ix = 5000000000_8, iy = -3000000000_8
    real(8), parameter :: rx = 2.5_8, ry = -7.125_8
    complex(8), parameter :: cx = (1.0_8, 2.0_8), cy = (3.0_8, -4.0_8)

    i4 = evaluate_i4(7, -3)
    if (any(i4 /= reshape([7, -3, 7, -3], [2, 2]))) error stop

    i8 = evaluate_i8(ix, iy)
    if (any(i8 /= reshape([ix, iy, ix, iy], [2, 2]))) error stop

    r4 = evaluate_r4(1.25, -3.5)
    if (any(abs(r4 - reshape([1.25, -3.5, 1.25, -3.5], [2, 2])) > 1e-6)) error stop

    r8 = evaluate_r8(rx, ry)
    if (any(abs(r8 - reshape([rx, ry, rx, ry], [2, 2])) > 1e-12_8)) error stop

    l = evaluate_l(.true., .false.)
    if (any(l .neqv. reshape([.true., .false., .true., .false.], [2, 2]))) error stop

    c8 = evaluate_c8(cx, cy)
    if (any(abs(c8 - reshape([cx, cy, cx, cy], [2, 2])) > 1e-12_8)) error stop
end program
