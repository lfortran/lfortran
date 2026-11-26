program bindc_module_variable_01
    use bindc_module_variable_01_m, only: shared_count, shared_char, &
        shared_chars, bump
    implicit none
    if (shared_count /= 5) error stop 1
    if (shared_char /= 'a') error stop 2
    if (shared_chars(2) /= 'b') error stop 3
    call bump
    if (shared_count /= 6) error stop 4
    if (shared_char /= 'b') error stop 5
    if (any(shared_chars /= ['a', 'b', 'd'])) error stop 6
end program
