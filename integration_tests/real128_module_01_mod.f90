module real128_module_01_mod
    implicit none
contains
    subroutine set_value(x)
        real(16), intent(out) :: x
        x = 1.0_16
    end subroutine set_value
end module real128_module_01_mod
