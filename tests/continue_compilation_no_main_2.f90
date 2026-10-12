! With --continue-compilation a valid file with no main program or global
! procedure still compiles as a library with -c and, without -c, is still
! reported as having no main program.
module continue_compilation_no_main_2_m
    implicit none
contains
    integer function g(n) result(r)
        integer, intent(in) :: n
        r = n + 1
    end function g
end module continue_compilation_no_main_2_m
