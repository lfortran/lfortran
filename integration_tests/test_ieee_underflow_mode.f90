program test_ieee_underflow_mode
    use ieee_arithmetic, only: ieee_set_underflow_mode, ieee_get_underflow_mode
    implicit none
    logical :: gradual

    call ieee_get_underflow_mode(gradual)
    if (.not. gradual) error stop

    call ieee_set_underflow_mode(.true.)
    call ieee_get_underflow_mode(gradual)
    if (.not. gradual) error stop

    call ieee_set_underflow_mode(.false.)
end program test_ieee_underflow_mode
