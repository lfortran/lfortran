module reachable_proc_symbols_mod
implicit none
contains
    integer function used(n)
        integer, intent(in) :: n
        used = n + 1
    end function used
    integer function unused(n)
        integer, intent(in) :: n
        unused = n + 2
    end function unused
end module reachable_proc_symbols_mod
