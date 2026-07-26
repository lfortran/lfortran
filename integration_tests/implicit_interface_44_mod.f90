subroutine implicit_interface_44_ode(f, neqn, y, t, tout, relerr, abserr, iflag, work, iwork)
    implicit none
    integer :: neqn, iflag, iwork(5)
    real(8) :: y(neqn), t, tout, relerr, abserr, work(100+21*neqn)
    external f
    real(8) :: yp(4)
    call f(t, y, yp)
    if (yp(1) /= 3.0_8) then
        error stop "Error in implicit_interface_44_ode"
    end if
    t = tout
end subroutine implicit_interface_44_ode
