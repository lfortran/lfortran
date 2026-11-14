program reachable_proc_symbols
    use reachable_proc_symbols_mod, only: used
    implicit none
    if (used(1) /= 2) error stop 1
end program reachable_proc_symbols
