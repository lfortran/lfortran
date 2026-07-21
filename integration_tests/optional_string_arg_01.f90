program optional_string_arg_01
    implicit none
    call check(1)
contains
    subroutine check(n, s)
        integer, intent(in) :: n
        character(len=n), optional, intent(in) :: s
        if (len(s) /= 1) error stop
    end subroutine
end program optional_string_arg_01
