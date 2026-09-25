module template_merge_01_m
    implicit none
    template choose_tmpl {t}
        deferred type :: t
    contains
        function choose(x, y, mask) result(r)
            type(t), intent(in) :: x, y
            logical, intent(in) :: mask
            type(t) :: r
            r = merge(x, y, mask)
        end function
    end template
end module

program template_merge_01
    use template_merge_01_m
    implicit none
    ! Experimental template syntax is not supported by GFortran.
    instantiate choose_tmpl {integer}, only: choose_i4 => choose
    instantiate choose_tmpl {real(8)}, only: choose_r8 => choose
    instantiate choose_tmpl {logical}, only: choose_l => choose

    template local_tmpl {t}
        deferred type :: t
    contains
        subroutine pick(x, y, mask, r)
            type(t), intent(in) :: x, y
            logical, intent(in) :: mask
            type(t), intent(out) :: r
            r = merge(x, y, mask)
        end subroutine
    end template
    instantiate local_tmpl {integer(8)}, only: pick_i8 => pick

    integer(8) :: k

    if (choose_i4(7, -3, .true.) /= 7) error stop
    if (choose_i4(7, -3, .false.) /= -3) error stop
    if (abs(choose_r8(2.5d0, -1.5d0, .true.) - 2.5d0) > 1d-12) error stop
    if (abs(choose_r8(2.5d0, -1.5d0, .false.) + 1.5d0) > 1d-12) error stop
    if (.not. choose_l(.true., .false., .true.)) error stop
    if (choose_l(.true., .false., .false.)) error stop
    call pick_i8(5000000000_8, -3_8, .true., k)
    if (k /= 5000000000_8) error stop
    call pick_i8(5000000000_8, -3_8, .false., k)
    if (k /= -3_8) error stop
    print *, "ok"
end program
