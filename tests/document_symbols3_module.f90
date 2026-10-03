module a
implicit none

integer :: x

contains

    subroutine f(n)
    integer, intent(in) :: n
    print *, n
    end subroutine

end module
