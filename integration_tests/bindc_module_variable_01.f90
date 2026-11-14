program bindc_module_variable_01
    use bindc_module_variable_01_m, only: shared_count, bump
    implicit none
    if (shared_count /= 5) error stop 1
    call bump
    if (shared_count /= 6) error stop 2
end program
