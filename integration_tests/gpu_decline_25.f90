! gpu decline reason: DummyProcedureUnbound
! the loop body hands a dummy procedure of the procedure it is in to another
! procedure: device code calls the routine a dummy procedure is handed
! directly, and which routine the host hands over is not known on the device.
module gpu_decline_25_m
    implicit none
contains
    pure real function apply(g, x)
        interface
            pure real function g(x)
                real, intent(in) :: x
            end function
        end interface
        real, intent(in) :: x
        apply = g(x)
    end function

    pure real function sq(x)
        real, intent(in) :: x
        sq = x * x
    end function

    subroutine fill(g, o)
        interface
            pure real function g(x)
                real, intent(in) :: x
            end function
        end interface
        real, intent(out) :: o(:)
        integer :: i
        do concurrent (i = 1:size(o))
            o(i) = apply(g, real(i))
        end do
    end subroutine
end module

program gpu_decline_25
    use gpu_decline_25_m, only: fill, sq
    implicit none
    real :: o(4)
    call fill(sq, o)
    if (abs(o(4) - 16) > 1e-5) error stop "bad o(4)"
    if (abs(o(1) - 1) > 1e-5) error stop "bad o(1)"
end program
