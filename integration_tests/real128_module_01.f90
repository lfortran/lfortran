program real128_module_01
    use real128_module_01_mod, only: set_value
    implicit none
    real(16) :: xl
    xl = 0.0_16
    call set_value(xl)
    print *, xl
    if (abs(xl - 1.0_16) > 1.0e-30_16) error stop
end program real128_module_01
