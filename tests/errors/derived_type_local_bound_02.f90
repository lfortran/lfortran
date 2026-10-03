program err2
    implicit none
    call s(1)
contains
    subroutine s(n)
        integer, intent(in) :: n
        type :: t
            integer :: c(n+1)
        end type
        type(t) :: x
        print *, size(x%c)
    end subroutine
end program
