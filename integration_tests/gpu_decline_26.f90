! gpu decline reason: DummyProcedureUnbound
! the loop body calls a procedure whose optional dummy procedure is absent:
! device code calls the routine a dummy procedure is handed directly, and
! no routine is handed over.
module gpu_decline_26_m
    implicit none
    abstract interface
        pure real function iface(x)
            real, intent(in) :: x
        end function
    end interface
contains
    pure real function apply(x, g)
        real, intent(in) :: x
        procedure(iface), optional :: g
        if (present(g)) then
            apply = g(x)
        else
            apply = x
        end if
    end function
end module

program gpu_decline_26
    use gpu_decline_26_m, only: apply
    implicit none
    real :: o(4)
    integer :: i
    do concurrent (i = 1:size(o))
        o(i) = apply(real(i))
    end do
    if (abs(o(4) - 4) > 1e-5) error stop "bad o(4)"
    if (abs(o(1) - 1) > 1e-5) error stop "bad o(1)"
end program
