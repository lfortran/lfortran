module quad_inf_sign_01_m
contains
    elemental subroutine set_inf(str, v)
        use, intrinsic :: ieee_arithmetic
        integer, parameter :: qp = selected_real_kind(33)
        character(*), intent(in) :: str
        real(qp), intent(inout) :: v
        integer, parameter :: nfnb = 64, nwnb = 50
        integer :: e
        real(qp), parameter :: whole(nwnb) = [(10._qp**(nwnb - e), e=1, nwnb)]
        real(qp), parameter :: frac(nfnb) = [(10._qp**(-e), e=1, nfnb)]
        real(qp), parameter :: expbase(nwnb + nfnb) = [whole, frac]
        integer(1) :: sign, ch
        sign = 1
        ch = iachar(str(1:1))
        if (ch == iachar('-')) then
            sign = -1
            ch = iachar(str(2:2))
        end if
        if (ch == iachar('I')) then
            v = sign * ieee_value(v, ieee_positive_inf)
            return
        end if
        v = expbase(54)
    end subroutine
end module

program quad_inf_sign_01
    use quad_inf_sign_01_m
    integer, parameter :: qp = selected_real_kind(33)
    real(qp) :: y
    y = 0
    call set_inf("Inf", y)
    if (.not. (y > huge(y))) error stop 1
    y = 0
    call set_inf("-Inf", y)
    if (.not. (y < -huge(y))) error stop 2
end program
