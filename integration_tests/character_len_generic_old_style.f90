module character_len_generic_old_style_m
    implicit none
    interface g
        procedure dbl
    end interface
contains
    pure integer function dbl(n)
        integer, intent(in) :: n
        dbl = 2*n
    end function
    subroutine s(n)
        integer, intent(in) :: n
        character :: str*(g(n))
        if (len(str) /= 6) error stop
        print *, len(str)
    end subroutine
end module
program character_len_generic_old_style
    use character_len_generic_old_style_m
    implicit none
    call s(3)
end program
