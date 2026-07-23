program module_real128_01
    use module_real128_01_mod, only: set_value
    implicit none
    real(16) :: xl
    xl = 0.0_16
    call set_value(xl)
    print *, xl
end program module_real128_01
