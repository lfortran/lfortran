program array_section_12
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    real(wp) :: default_colors(3, 5)
    real(wp) :: plot_color(3)
    integer :: i

    do i = 1, 5
        default_colors(:, i) = [real(i, wp), real(i + 10, wp), real(i + 20, wp)]
    end do

    call pick_color(default_colors, 4, plot_color)

    if (any(abs(plot_color - [4.0_wp, 14.0_wp, 24.0_wp]) > 1e-12_wp)) error stop
    print *, "PASS"
contains
    subroutine pick_color(colors, idx, out_color)
        real(wp), intent(in) :: colors(:,:)
        integer, intent(in) :: idx
        real(wp), intent(out) :: out_color(3)

        out_color = colors(:, idx)
    end subroutine pick_color
end program array_section_12
