! With --continue-compilation a file with no main program or global procedure
! whose module has an error must fail, with or without -c, reporting only that
! error: no missing main program and no library built from it.
module continue_compilation_no_main_1_m
    implicit none
contains
    integer function g(n) result(r)
        integer, intent(in) :: n
        r = n + undefined_in_module
    end function g
end module continue_compilation_no_main_1_m
