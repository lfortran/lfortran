program spec_expr_10
    ! rank and a type inquiry of a local in a specification expression
    implicit none
    call s()
contains
    subroutine s()
        integer :: a(2, 3)
        real :: x
        integer :: c(rank(a))
        character(len=digits(x)) :: str
        print *, size(c), len(str)
        if (size(c) /= 2) error stop
        if (len(str) /= 24) error stop
    end subroutine
end program
