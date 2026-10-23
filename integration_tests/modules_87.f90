! A generic subroutine and a generic function whose specifics are private,
! imported with `use, only:`.
module modules_87_m
    implicit none
    private
    public :: show, twice
    interface show
        module procedure show_int
    end interface
    interface twice
        module procedure twice_int
    end interface
contains
    subroutine show_int(x, y)
        integer, intent(in) :: x
        integer, intent(out) :: y
        y = x + 1
    end subroutine
    integer function twice_int(x)
        integer, intent(in) :: x
        twice_int = 2*x
    end function
end module

program modules_87
    use modules_87_m, only: show, twice
    implicit none
    integer :: y
    call show(1, y)
    print *, y, twice(3)
    if (y /= 2) error stop 1
    if (twice(3) /= 6) error stop 2
end program
